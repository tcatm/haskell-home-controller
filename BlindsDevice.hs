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
    { position :: Double
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
    watchDPT1 (upDownGA config) upDownHandler
    watchDPT1 (stopGA config) stopHandler
    watchDPT5_1 (positionGA config) positionHandler

    where
        upDownHandler False = do
            debug "Received up command"
            moveTo 0 True

        upDownHandler True = do
            debug "Received down command"
            moveTo 1 True

        stopHandler _ = do
            debug "Received stop command"
            changeState Idle

        positionHandler pos = do
            debug $ "Received position " ++ show pos
            moveTo pos False

        setBlindState :: BlindState -> DeviceM BlindsState ()
        setBlindState newState = modify $ \s -> s { blindState = newState }

        setPosition :: Double -> DeviceM BlindsState ()
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

        calcPosition :: NominalDiffTime -> NominalDiffTime -> Double
        calcPosition timeToMove time = realToFrac (time / timeToMove)

        remainingTimeF :: Double -> NominalDiffTime -> NominalDiffTime
        remainingTimeF pos timeToMove = realToFrac (pos * (realToFrac timeToMove))

        moveTo :: Double -> Bool -> DeviceM BlindsState ()
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

        moveTo' :: Double -> Bool -> DeviceM BlindsState ()
        moveTo' pos fullmove = do
            position' <- gets position
            let direction = if pos > position' then MovingDown else MovingUp
            let delta = abs $ pos - position'

            let timeToMove' = timeToMove config
            let remainingTime = case fullmove of
                                    True -> timeToMove' + motorStartDelay config
                                    False -> (remainingTimeF delta timeToMove') + motorStartDelay config

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
                groupWrite (positionStateGA config) (DPT5_1 newPosition)
                setPosition newPosition