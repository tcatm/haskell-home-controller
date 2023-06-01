{-# LANGUAGE DeriveGeneric #-}

module BlindsDevice
    ( BlindsConfig (..)
    , makeBlindsDevice
    ) where

import Device
import DeviceTypes
import KNXAddress
import DPTs
import Data.Word
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Maybe
import Control.Monad
import GHC.Generics
import Data.Aeson

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

data BlindState = Idle | MovingUp | MovingDown deriving (Show, Generic)

instance ToJSON BlindState

data BlindsState = BlindsState
    { position :: Double
    , blindState :: BlindState
    , lastMove :: Maybe UTCTime
    , timerId :: Maybe TimerId
    } deriving (Show, Generic)

instance ToJSON BlindsState

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
        upDownHandler False = debug "Received up command" >> moveTo 0 True
        upDownHandler True = debug "Received down command" >> moveTo 1 True
        stopHandler _ = do
            debug "Received stop command" 
            newPosition <- stop
            maybe (return ()) updatePosition newPosition

        positionHandler pos = do
            debug $ "Received position " <> show pos
            moveTo pos False

        setBlindState newState = modify $ \s -> s { blindState = newState }
        setPosition newPos = modify $ \s -> s { position = newPos }
        setLastMove newLM = modify $ \s -> s { lastMove = newLM }
        setTimerId newId = modify $ \s -> s { timerId = newId }

        stop = do
            state <- gets blindState
            case state of
                Idle -> return Nothing
                _ -> do
                    debug "Stopping"
                    case state of
                        MovingUp -> groupWrite (openGA config) (DPT1 True)
                        MovingDown -> groupWrite (closeGA config) (DPT1 True)
                    setBlindState Idle
                    newPosition <- calculatePosition state
                    setLastMove Nothing
                    timerId <- gets timerId
                    unless (isNothing timerId) $ do
                        cancelTimer (fromJust timerId)
                        setTimerId Nothing

                    return $ Just newPosition

        remainingTimeF pos timeToMove = realToFrac (pos * (realToFrac timeToMove))

        moveTo :: Double -> Bool -> DeviceM BlindsState ()
        moveTo pos fullmove = do
            debug $ "Moving to " <> show pos

            oldState <- gets blindState
            case oldState of
                Idle -> moveTo' pos fullmove
                _ -> do
                    stop
                    scheduleIn 1 $
                        moveTo' pos fullmove
                    return ()

        moveTo' :: Double -> Bool -> DeviceM BlindsState ()
        moveTo' pos fullmove = do
            let fullmove' = fullmove || pos == 0 || pos == 1
            position' <- gets position
            let direction = if pos > position' then MovingDown else MovingUp
            let delta = abs $ pos - position'

            let timeToMove' = timeToMove config
            let remainingTime = case fullmove' of
                                    True -> timeToMove' + motorStartDelay config
                                    False -> (remainingTimeF delta timeToMove') + motorStartDelay config

            debug $ "Fullmove: " <> show fullmove'

            debug $ "position': " <> show position' <> ", direction: " <> show direction

            debug $ "Delta: " <> show delta <> ", timeToMove: " <> show timeToMove' <> ", remainingTime: " <> show remainingTime

            -- Move only if remaining time is greater than 1 second
            when (remainingTime > 1) $ do
                now <- zonedTimeToUTC <$> getTime
                case direction of
                    MovingUp -> groupWrite (openGA config) (DPT1 True)
                    MovingDown -> groupWrite (closeGA config) (DPT1 True)
                setLastMove $ Just now
                setBlindState direction
                scheduleStop direction remainingTime pos
        
        scheduleStop blindState remainingTime pos = do
            timerId <- gets timerId
            unless (isNothing timerId) $ do
                cancelTimer (fromJust timerId)
                setTimerId Nothing

            debug $ "Remaining time: " <> show remainingTime

            when (remainingTime > 0) $ do
                debug $ "Scheduling stop in " <> show remainingTime <> " seconds"
                timerId <- scheduleIn remainingTime $ do
                    debug "Timer fired"
                    stop
                    updatePosition pos

                setTimerId (Just timerId)

        calculatePosition blindState = do
            lastMove <- gets lastMove
            oldPosition <- gets position
            case lastMove of
                Nothing -> return oldPosition
                _ -> do
                    now <- zonedTimeToUTC <$> getTime
                    let time = diffUTCTime now (fromJust lastMove)
                    debug $ "Time since last move: " <> show time
                    let timeToMove' = timeToMove config
                    debug $ "Time to move: " <> show timeToMove'
                    debug $ "Old position: " <> show oldPosition

                    let deltaPosition = realToFrac (time / timeToMove')

                    let newPosition = case blindState of
                            MovingUp -> oldPosition - deltaPosition
                            MovingDown -> oldPosition + deltaPosition
                            Idle -> oldPosition

                    debug $ "New position: " <> show newPosition

                    return newPosition
                
        updatePosition pos = do
            debug $ "Updating position to " <> show pos
            groupWrite (positionStateGA config) (DPT5_1 pos)
            setPosition pos