module BlindsDevice
    ( BlindsConfig (..)
    , makeBlindsDevice
    ) where

import Device
import KNXAddress
import DPTs
import Data.Word
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Maybe
import Control.Monad

data BlindsConfig = BlindsConfig
    { upDownGA :: GroupAddress
    , stopGA :: GroupAddress
    , positionGA :: GroupAddress
    , positionStateGA :: GroupAddress
    , openGA :: GroupAddress
    , closeGA :: GroupAddress
    , timeToMove :: NominalDiffTime
    , motorStartDelay :: NominalDiffTime
    } deriving (Show)

data BlindState = Idle | MovingUp | MovingDown deriving (Show)

data BlindsState = BlindsState
    { position :: Word8
    , blindState :: BlindState
    , lastMove :: Maybe UTCTime
    , timerId :: Maybe TimerId
    } deriving (Show)

initialBlindsState = BlindsState
    { position = 0
    , blindState = Idle
    , lastMove = Nothing
    , timerId = Nothing
    }

makeBlindsDevice :: String -> BlindsConfig -> Device
makeBlindsDevice name config = makeDevice name initialBlindsState $ blindsDeviceF config

blindsDeviceF :: BlindsConfig -> DeviceM BlindsState ()
blindsDeviceF config = do
    eventLoop (groupValue (upDownGA config) parseDPT1) upDownHandler
    eventLoop (groupValue (stopGA config) parseDPT1) stopHandler
    eventLoop (groupValue (positionGA config) parseDPT5) positionHandler

    where
        upDownHandler (DPT1 False) = do
            debug "Received up command"
            moveTo 0

        upDownHandler (DPT1 True) = do
            debug "Received down command"
            moveTo 255

        stopHandler (DPT1 _) = do
            debug "Received stop command"
            changeState Idle

        positionHandler (DPT5 pos) = do
            debug $ "Received position " ++ show pos
            moveTo pos
        
        changeState newState = do
            oldState <- gets blindState
            case (oldState, newState) of
                (Idle, MovingUp) -> do
                    debug "Starting up"
                    sendEvent (openGA config) True
                    startTimer config
                    modify $ \s -> s { blindState = MovingUp }
                (Idle, MovingDown) -> do
                    debug "Starting down"
                    sendEvent (closeGA config) True
                    startTimer config
                    modify $ \s -> s { blindState = MovingDown }
                (MovingUp, Idle) -> do
                    debug "Stopping up"
                    sendEvent (openGA config) True
                    updatePosition MovingUp
                    stopTimer config
                    cancelStop
                    modify $ \s -> s { blindState = Idle }
                (MovingDown, Idle) -> do
                    debug "Stopping down"
                    sendEvent (closeGA config) True
                    updatePosition MovingDown
                    stopTimer config
                    cancelStop
                    modify $ \s -> s { blindState = Idle }
                (MovingUp, MovingDown) -> do
                    debug "Stopping up, starting down"
                    sendEvent (closeGA config) True
                    stopTimer config
                    updatePosition MovingUp
                    cancelStop
                    startTimer config
                    modify $ \s -> s { blindState = MovingDown }
                (MovingDown, MovingUp) -> do
                    debug "Stopping down, starting up"
                    sendEvent (openGA config) True
                    stopTimer config
                    updatePosition MovingDown
                    cancelStop
                    startTimer config
                    modify $ \s -> s { blindState = MovingUp }
                _ -> return ()

        calcPosition :: NominalDiffTime -> NominalDiffTime -> Word8
        calcPosition timeToMove time = round $ (time / timeToMove) * 255

        remaingTimeF :: Integer -> NominalDiffTime -> NominalDiffTime
        remaingTimeF pos timeToMove = (fromIntegral pos / 255) * timeToMove

        moveTo :: Word8 -> DeviceM BlindsState ()
        moveTo pos = do
            debug $ "Moving to " ++ show pos
            changeState Idle

            position' <- gets position
            let direction = if pos > position' then MovingDown else MovingUp
            let delta = abs $ (fromIntegral pos :: Integer) - (fromIntegral position' :: Integer)

            let timeToMove' = timeToMove config
            let remainingTime = (remaingTimeF delta timeToMove') + motorStartDelay config

            debug $ "position': " ++ show position' ++ ", direction: " ++ show direction

            debug $ "Delta: " ++ show delta ++ ", timeToMove: " ++ show timeToMove' ++ ", remainingTime: " ++ show remainingTime

            when (remainingTime > 0) $ do
                changeState direction
                scheduleStop direction remainingTime pos

        cancelStop = do
            timerId <- gets timerId
            case timerId of
                Nothing -> return ()
                Just timerId' -> do
                    cancelTimer timerId'
                    modify $ \s -> s { timerId = Nothing }
        
        scheduleStop blindState remainingTime pos = do
            timerId <- gets timerId
            unless (isNothing timerId) $ do
                cancelTimer (fromJust timerId)
                modify $ \s -> s { timerId = Nothing }

            debug $ "Remaining time: " ++ show remainingTime

            when (remainingTime > 0) $ do
                debug $ "Scheduling stop in " ++ show remainingTime ++ " seconds"
                timerId <- scheduleIn remainingTime $ do
                    debug "Timer fired"
                    changeState Idle
                    modify $ \s -> s { position = pos }

                modify $ \s -> s { timerId = Just timerId }

        updatePosition blindState = do
            lastMove <- gets lastMove
            case lastMove of
                Nothing -> return ()
                Just lastMove' -> do
                    now <- zonedTimeToUTC <$> getTime
                    let time = diffUTCTime now lastMove'
                    debug $ "Time since last move: " ++ show time
                    let timeToMove' = timeToMove config
                    debug $ "Time to move: " ++ show timeToMove'
                    oldPosition <- gets position
                    debug $ "Old position: " ++ show oldPosition

                    let deltaPosition = calcPosition timeToMove' time

                    let newPosition = case blindState of
                            MovingUp -> oldPosition - deltaPosition
                            MovingDown -> oldPosition + deltaPosition
                            Idle -> oldPosition

                    debug $ "New position: " ++ show newPosition
                    groupWrite (positionStateGA config) (DPT5 newPosition)
                    modify $ \s -> s { position = newPosition }
                        
        startTimer config = do
            lastMove <- gets lastMove
            debug "Starting timer"
            now <- zonedTimeToUTC <$> getTime
            modify $ \s -> s { lastMove = Just now }

        stopTimer config = do
            lastMove <- gets lastMove
            case lastMove of
                Nothing -> return ()
                Just _ -> do
                    debug "Stopping timer"
                    modify $ \s -> s { lastMove = Nothing }
        
        sendEvent ga value = do
            debug $ "Sending event " ++ show value ++ " to " ++ show ga
            groupWrite ga (DPT1 value)

