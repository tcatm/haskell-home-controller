{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FM2 ( config )
where

import KNXAddress
import KNXDatatypes
import DPTs
import Device
import DeviceTypes
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
                , hueScenes
                , szeneGesamt
                , poolDemultiplexer
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
    , sceneMultiplexerEntries = [ SceneMultiplexerEntry (GroupAddress 0 1 3) 0 4    -- HWR
                                , SceneMultiplexerEntry (GroupAddress 0 1 4) 4 4    -- Küche
                                , SceneMultiplexerEntry (GroupAddress 0 1 5) 8 4    -- Wohnzimmer
                                , SceneMultiplexerEntry (GroupAddress 0 1 2) 12 4   -- Diele
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

hueScenesConfig = [ (GroupAddress 0 1 18, "Hobbyraum")
                  , (GroupAddress 0 1 12, "Pool")
                  , (GroupAddress 0 1 13, "Wintergarten")
                  , (GroupAddress 0 1 9, "Hauptbad")
                  ]

hueScenes :: Device
hueScenes = makeDevice "Hue Szenen" () $ hueScenesF hueScenesConfig

hueScenesF :: [(GroupAddress, String)] -> DeviceM () ()
hueScenesF config = do
    forM_ config $ \(ga, name) -> do
        watchDPT18_1 ga $ \(save, scene) -> unless save $ do
            case scene of
                0 -> do
                    debug "Hue: Off"
                    hueSetRoomOn name False
                _ -> do
                    debug $ "Hue scene: " <> show scene
                    let sceneString = show scene

                    hueActivateScene name sceneString


szeneGesamt :: Device
szeneGesamt = makeDevice "Szene Gesamt" () $ szeneGesamtF

szeneGesamtF :: DeviceM () ()
szeneGesamtF = do
    let gas = [ GroupAddress 0 1 i | i <- [2..23] ]
    watchDPT18_1 (GroupAddress 0 1 1) $ \(save, scene) -> unless save $ do
        debug $ "Szene Gesamt: " <> show scene
        forM_ gas $ \ga -> groupWrite ga $ DPT18_1 (False, scene)

poolDemultiplexer :: Device
poolDemultiplexer = makeDevice "Pool Demultiplexer" Map.empty poolDemultiplexerF

poolDemultiplexerF :: DeviceM (Map.Map Int Bool) ()
poolDemultiplexerF = do
    let inputGA = GroupAddress 3 1 0
    let items = [106, 107, 101, 108, 102, 109, 104, 103]
    let statusGAs = [ GroupAddress 5 2 i | i <- items ]
    let controlGAs = [ GroupAddress 5 1 i | i <- items ]

    -- forM_ with index
    forM_ (zip [0..] statusGAs) $ \(i, ga) -> do
        watchDPT1 ga $ \value -> do
            modify $ Map.insert i value

    watchDPT5 inputGA $ \value -> do
        let value' = fromIntegral value
        unless (value' < 0 || value' >= length controlGAs) $ do
            let ga = controlGAs !! value'
            state <- gets $ Map.findWithDefault False value'
            groupWrite ga $ DPT1 $ not state