module FM2 ( config )
where

import KNXAddress
import KNXDatatypes
import DPTs
import Device
import Config

import BlindsDevice
import StaircaseLight
import TimeSender

import qualified Data.Map as Map
import Control.Monad

config = Config
    { devices = [ timeSender timeSenderConfig
                , presenceDevice
                ]
    }

timeSenderConfig = TimeSenderConfig
    { timeGA = GroupAddress 0 0 1
    , dateGA = GroupAddress 0 0 2
    , intervalSeconds = 300
    }

data PresenceDeviceState = PresenceDeviceState
    { presence :: Maybe Bool
    , presenceTimer :: Maybe TimerId
    } deriving (Show)

presenceDeviceInitialState = PresenceDeviceState
    { presence = Nothing
    , presenceTimer = Nothing
    }

presenceDevice :: Device
presenceDevice = makeDevice "Anwesenheit" presenceDeviceInitialState presenceDeviceF

presenceDeviceF :: DeviceM PresenceDeviceState ()
presenceDeviceF = do
    let presenceGA = GroupAddress 0 0 3

    respondOnRead presenceGA $ fmap DPT1 <$> gets presence

    watchDPT1 presenceGA $ \presence -> do
        debug $ "Presence: " <> show presence
        case presence of
            True -> enablePresence
            False -> disablePresence

        modify $ \s -> s { presence = Just presence }

enablePresence = do
    
    timerId <- gets presenceTimer
    case timerId of
        Just timerId -> do 
            cancelTimer timerId
            modify $ \s -> s { presenceTimer = Nothing }
        Nothing -> return ()

disablePresence = do

    let timerDelay = 3 * 24 * 60 * 60
    timerId <- scheduleIn timerDelay $ do
        -- TODO Add instructions for powering down the house after a few days of absence
        return ()

    modify $ \s -> s { presenceTimer = Just timerId }
