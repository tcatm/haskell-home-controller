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
            moveTo 0 True

        upDownHandler (DPT1 True) = do
            debug "Received down command"
            moveTo 255 True

        stopHandler (DPT1 _) = do
            debug "Received stop command"
            changeState Idle

        positionHandler (DPT5 pos) = do
            debug $ "Received position " ++ show pos
            moveTo pos False

        setBlindState :: BlindState -> DeviceM BlindsState ()
        setBlindState newState = modify $ \s -> s { blindState = newState }

        setPosition :: Word8 -> DeviceM BlindsState ()
        setPosition newPos = modify $ \s -> s { position = newPos }

        setLastMove :: Maybe UTCTime -> DeviceM BlindsState ()
        setLastMove newLM = modify $ \s -> s { lastMove = newLM }

        setTimerId :: Maybe TimerId -> DeviceM BlindsState ()
        setTimerId newId = modify $ \s -> s { timerId = newId }

        changeState newState = do
            oldState <- gets blindState
            now <- zonedTimeToUTC <$> getTime
            setBlindState newState
            case (oldState, newState) of
                (Idle, MovingUp) -> do
                    debug "Starting up"
                    groupWrite (openGA config) (DPT1 True)
                    setLastMove (Just now)
                (Idle, MovingDown) -> do
                    debug "Starting down"
                    groupWrite (closeGA config) (DPT1 True)
                    setLastMove (Just now)
                (_, Idle) -> do
                    debug "Stopping up"
                    updatePosition oldState
                    case oldState of
                        MovingUp -> groupWrite (openGA config) (DPT1 True)
                        MovingDown -> groupWrite (closeGA config) (DPT1 True)
                    timerId <- gets timerId
                    unless (isNothing timerId) $ do
                        cancelTimer (fromJust timerId)
                        setTimerId Nothing
                    setLastMove Nothing
                _ -> return ()

        calcPosition :: NominalDiffTime -> NominalDiffTime -> Word8
        calcPosition timeToMove time = round $ (time / timeToMove) * 255

        remaingTimeF :: Integer -> NominalDiffTime -> NominalDiffTime
        remaingTimeF pos timeToMove = (fromIntegral pos / 255) * timeToMove

        moveTo :: Word8 -> Bool -> DeviceM BlindsState ()
        moveTo pos fullmove = do
            debug $ "Moving to " ++ show pos

            oldState <- gets blindState
            case oldState of
                Idle -> moveTo' pos fullmove
                _ -> do
                    changeState Idle
                    scheduleIn 1 $
                        moveTo' pos fullmove
                    return ()

        moveTo' :: Word8 -> Bool -> DeviceM BlindsState ()
        moveTo' pos fullmove = do
            position' <- gets position
            let direction = if pos > position' then MovingDown else MovingUp
            let delta = abs $ (fromIntegral pos :: Integer) - (fromIntegral position' :: Integer)

            let timeToMove' = timeToMove config
            let remainingTime = case fullmove of
                                    True -> timeToMove' + motorStartDelay config
                                    False -> (remaingTimeF delta timeToMove') + motorStartDelay config

            debug $ "position': " ++ show position' ++ ", direction: " ++ show direction

            debug $ "Delta: " ++ show delta ++ ", timeToMove: " ++ show timeToMove' ++ ", remainingTime: " ++ show remainingTime

            when (remainingTime > 0) $ do
                changeState direction
                scheduleStop direction remainingTime pos
        
        scheduleStop blindState remainingTime pos = do
            timerId <- gets timerId
            unless (isNothing timerId) $ do
                cancelTimer (fromJust timerId)
                setTimerId Nothing

            debug $ "Remaining time: " ++ show remainingTime

            when (remainingTime > 0) $ do
                debug $ "Scheduling stop in " ++ show remainingTime ++ " seconds"
                timerId <- scheduleIn remainingTime $ do
                    debug "Timer fired"
                    changeState Idle
                    setPosition pos

                setTimerId (Just timerId)

        updatePosition blindState = do
            lastMove <- gets lastMove
            unless (isNothing lastMove) $ do
                now <- zonedTimeToUTC <$> getTime
                let time = diffUTCTime now (fromJust lastMove)
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
                setPosition newPosition