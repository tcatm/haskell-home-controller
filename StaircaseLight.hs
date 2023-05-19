{-# LANGUAGE ExistentialQuantification #-}

module StaircaseLight 
    ( staircaseLight
    , StaircaseLightConfig(..)
    ) where

import Device
import KNXAddress
import DPTs
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad

data LightState = LightState    { lightOn :: Bool
                                , timerOn :: Bool
                                , timerId :: Maybe TimerId
                                , lightOnTime :: Maybe UTCTime
                                } deriving (Show)

data StaircaseLightConfig = StaircaseLightConfig
    { lightOnAddress :: GroupAddress
    , lightOffAddress :: GroupAddress
    , lightOffTime :: NominalDiffTime
    } deriving (Show)

initialLightState :: LightState
initialLightState = LightState False False Nothing Nothing

staircaseLight :: String -> StaircaseLightConfig -> Device
staircaseLight name config = makeDevice name initialLightState (startDevice config)

startDevice :: StaircaseLightConfig -> DeviceM LightState ()
startDevice config = watchDPT1 (lightOnAddress config) (handleLightSwitch (lightOffAddress config) (lightOffTime config))

handleLightSwitch :: GroupAddress -> NominalDiffTime -> Bool -> DeviceM LightState ()
handleLightSwitch lightOffAddress lightOffTime state = do
    currentTime <- zonedTimeToUTC <$> getTime
    -- if state is true and the light is not already on, remember the time
    previousState <- gets lightOn
    when (state && not previousState) $
        modify $ \s -> s { lightOnTime = Just currentTime }
    modify $ \s -> s { lightOn = state }
    oldTimerId <- gets timerId
    whenJust oldTimerId cancelTimer
    modify $ \s -> s { timerOn = False }
    when state $ do
        newTimerId <- scheduleIn lightOffTime $ turnOffLight lightOffAddress
        modify $ \s -> s { timerOn = True, timerId = Just newTimerId }

turnOffLight :: GroupAddress -> DeviceM LightState ()
turnOffLight lightOffAddress = do
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