{-# LANGUAGE ExistentialQuantification #-}

module StaircaseLight 
    ( staircaseLight
    ) where

import Device
import KNXAddress
import DPTs
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad

data LightState = LightState { lightOn :: Bool, timerOn :: Bool, timerId :: Maybe TimerId, lightOnTime :: Maybe UTCTime }
    deriving (Show)

lightOnAddress :: GroupAddress
lightOnAddress = GroupAddress 1 1 9

lightOffAddress :: GroupAddress
lightOffAddress = GroupAddress 1 1 9
lightOffTime :: NominalDiffTime
lightOffTime = 5

initialLightState :: LightState
initialLightState = LightState False False Nothing Nothing

staircaseLight :: SomeDevice
staircaseLight = makeDevice "StaircaseLight" initialLightState startDevice

startDevice :: DeviceM LightState ()
startDevice = do
    groupRead lightOnAddress parseDPT1 handleLightSwitch

handleLightSwitch :: DPT -> DeviceM LightState ()
handleLightSwitch dpt = do
    case dpt of
        DPT1 state -> do
            currentTime <- zonedTimeToUTC <$> getTime
            -- if state is true and the light is not already on, remember the time
            previousState <- gets lightOn
            when (state && not previousState) $
                modify $ \s -> s { lightOnTime = Just currentTime }
            modify $ \s -> s { lightOn = state }
            when state $ do
                oldTimerId <- gets timerId
                whenJust oldTimerId cancelTimer
                newTimerId <- scheduleIn lightOffTime turnOffLight
                modify $ \s -> s { timerOn = True, timerId = Just newTimerId }
        _ -> return ()
    startDevice -- This will keep the loop going.

turnOffLight :: DeviceM LightState ()
turnOffLight = do
    currentTime <- zonedTimeToUTC <$> getTime
    maybeOnTime <- gets lightOnTime
    case maybeOnTime of
        Just onTime -> do
            let duration = diffUTCTime currentTime onTime
            debug $ "Light was on for " ++ show duration
        Nothing -> return ()
    groupWrite lightOffAddress (DPT1 False)
    modify $ \s -> s { lightOn = False, timerOn = False, timerId = Nothing, lightOnTime = Nothing }

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x