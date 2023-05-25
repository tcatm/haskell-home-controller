{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

import Data.Aeson
import qualified Data.Map as Map
import Control.Monad
import GHC.Generics

config = Config
    { devices = [ timeSender timeSenderConfig
                , presenceDevice
                , sceneMultiplexer
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
    } deriving (Show, Generic)

instance ToJSON PresenceDeviceState

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

data SceneMultiplexerEntry = SceneMultiplexerEntry
    { sceneInputGA :: GroupAddress
    , sceneInputStart :: Int
    , sceneInputCount :: Int
    } deriving (Show)

data SceneMultiplexerConfig = SceneMultiplexerConfig
    { sceneMultiplexerOutputGA :: GroupAddress
    , sceneMultiplexerEntries :: [SceneMultiplexerEntry]
    } deriving (Show)

sceneMultiplexerConfig = SceneMultiplexerConfig
    { sceneMultiplexerOutputGA = GroupAddress 0 1 0
    , sceneMultiplexerEntries = [ SceneMultiplexerEntry (GroupAddress 0 1 3) 0 3    -- HWR
                                , SceneMultiplexerEntry (GroupAddress 0 1 4) 3 4    -- Küche
                                , SceneMultiplexerEntry (GroupAddress 0 1 5) 7 4    -- Wohnzimmer
                                , SceneMultiplexerEntry (GroupAddress 0 1 2) 11 4   -- Diele
                                ]
    }

sceneMultiplexer :: Device
sceneMultiplexer = makeDevice "Szenenmultiplexer" () (sceneMultiplexerF sceneMultiplexerConfig)

sceneMultiplexerF :: SceneMultiplexerConfig -> DeviceM () ()
sceneMultiplexerF config = do
    forM_ (sceneMultiplexerEntries config) $ \entry -> do
        let inputGA = sceneInputGA entry
        watchDPT18_1 inputGA $ \(save, scene) -> do
            debug $ "Scene: " <> show scene
            let outputGA = sceneMultiplexerOutputGA config

            when (scene < sceneInputCount entry) $ do
                let outputValue = scene + sceneInputStart entry
                debug $ "Mapped to " <> show outputValue <> " on " <> show outputGA <> " (save: " <> show save <> ")"
                groupWrite outputGA $ DPT18_1 (save, outputValue)