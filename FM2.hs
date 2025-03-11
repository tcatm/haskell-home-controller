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
import Data.Maybe (listToMaybe)
import Control.Monad
import GHC.Generics

config = Config
    { devices = [ timeSender timeSenderConfig
                , presenceDevice
                , hueScenes
                , szeneGesamt
                , szeneHausOhneOG
                , poolDemultiplexer
                , daliScenes
                , garagenlicht
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

hueScenesConfig = [ (GroupAddress 0 1 18, "Hobbyraum")
                  , (GroupAddress 0 1 12, "Pool")
                  , (GroupAddress 0 1 13, "Wintergarten")
                  , (GroupAddress 0 1 9, "Hauptbad")
                  , (GroupAddress 0 1 20, "Ankleide")
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
    let gas = [ GroupAddress 0 1 i | i <- [2..15] ++ [17..23] ]
    watchDPT18_1 (GroupAddress 0 1 1) $ \(save, scene) -> unless save $ do
        debug $ "Szene Gesamt: " <> show scene
        forM_ gas $ \ga -> groupWrite ga $ DPT18_1 (False, scene)

szeneHausOhneOG :: Device
szeneHausOhneOG = makeDevice "Szene Haus ohne OG" () $ do
    let gas = [ GroupAddress 0 1 i | i <- [2..8] ++ [10..13] ++ [18..19] ++ [21..23] ++ [50] ]
    watchDPT18_1 (GroupAddress 0 1 100) $ \(save, scene) -> unless save $ do
        debug $ "Szene Haus ohne OG: " <> show scene
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

-- Ignore, Off or Dim with double value
data DaliSceneState = Ignore | Off | Dim Double deriving (Show, Generic)

data DaliSceneMapEntry = DaliSceneMapEntry
    { daliSceneMapEntryInputGA:: GroupAddress
    , daliSceneMapEntrySwitch :: GroupAddress
    , daliSceneMapEntryDim :: GroupAddress
    , daliSceneMapEntryScenes :: [DaliSceneState]
    } deriving (Show, Generic)

daliScenesConfig =  [ DaliSceneMapEntry (GroupAddress 0 1 2) (GroupAddress 1 1 27) (GroupAddress 1 3 27)
                        [Off, Dim 1, Ignore, Dim 0.05] -- Diele
                    , DaliSceneMapEntry (GroupAddress 0 1 3) (GroupAddress 1 1 0) (GroupAddress 1 3 0)
                        [Off, Dim 1, Ignore, Dim 0.05] -- HWR
                    , DaliSceneMapEntry (GroupAddress 0 1 4) (GroupAddress 1 1 24) (GroupAddress 1 3 24)
                        [Off, Dim 0.8, Ignore, Dim 0.05] -- Küche
                    , DaliSceneMapEntry (GroupAddress 0 1 5) (GroupAddress 1 1 25) (GroupAddress 1 3 25)
                        [Off, Dim 0.7, Ignore, Dim 0.02] -- Wohnzimmer
                    , DaliSceneMapEntry (GroupAddress 0 1 5) (GroupAddress 1 1 29) (GroupAddress 1 3 29)
                        [Off, Dim 0.8, Ignore, Dim 0.01] -- Wohnzimmer
                    , DaliSceneMapEntry (GroupAddress 0 1 22) (GroupAddress 1 1 26) (GroupAddress 1 3 26)
                        [Off, Dim 0.8, Ignore, Off] -- Bibliothek
                    ]

daliScenes :: Device
daliScenes = makeDevice "DALI Scenes" () (daliScenesF daliScenesConfig)

daliScenesF :: [DaliSceneMapEntry] -> DeviceM () ()
daliScenesF config = do
    forM_ config $ \entry -> do
        let inputGA = daliSceneMapEntryInputGA entry
        watchDPT18_1 inputGA $ \(save, scene) -> do
            debug $ "Scene: " <> show scene
            let switch = daliSceneMapEntrySwitch entry
            let dim = daliSceneMapEntryDim entry
            let state = listToMaybe . drop scene $ daliSceneMapEntryScenes entry

            case state of
                Just Ignore -> return ()
                Just Off -> groupWrite switch $ DPT1 False
                Just (Dim v) -> do
                    groupWrite dim $ DPT5_1 v
                Nothing -> return ()

-- | State for the Garagenlicht device.
data GaragenlichtState = GaragenlichtState
    { insideDoor :: Bool   -- ^ State of the inside door (0/7/3)
    , outsideDoor :: Bool  -- ^ State of the outside door (0/7/4)
    , lastScene :: Int     -- ^ Last known scene (from 0/1/11)
    } deriving (Show, Generic)

instance ToJSON GaragenlichtState

-- | Initial state: both doors closed and light off (scene 0).
garagenlichtInitialState :: GaragenlichtState
garagenlichtInitialState = GaragenlichtState
    { insideDoor = False
    , outsideDoor = False
    , lastScene = 0
    }

-- | The Garagenlicht device.
garagenlicht :: Device
garagenlicht = makeDevice "Garagenlicht" garagenlichtInitialState garagenlichtF

-- | Device behavior function.
garagenlichtF :: DeviceM GaragenlichtState ()
garagenlichtF = do
    let insideDoorGA  = GroupAddress 0 7 3
    let outsideDoorGA = GroupAddress 0 7 4
    let sceneGA       = GroupAddress 0 1 11

    -- Watch the inside door sensor.
    watchDPT1 insideDoorGA $ \state -> do
        debug $ "Garagenlicht: inside door state: " <> show state
        modify $ \s -> s { insideDoor = state }
        checkAndSetScene sceneGA

    -- Watch the outside door sensor.
    watchDPT1 outsideDoorGA $ \state -> do
        debug $ "Garagenlicht: outside door state: " <> show state
        modify $ \s -> s { outsideDoor = state }
        checkAndSetScene sceneGA

    -- Watch the scene input to keep the last scene updated.
    watchDPT18_1 sceneGA $ \(save, scene) -> unless save $ do
        debug $ "Garagenlicht: scene input: " <> show scene
        modify $ \s -> s { lastScene = scene }
        checkAndSetScene sceneGA

-- | Check the current state and update the scene if needed.
checkAndSetScene :: GroupAddress -> DeviceM GaragenlichtState ()
checkAndSetScene sceneGA = do
    st <- gets id
    let doorsOpen    = insideDoor st || outsideDoor st
        currentScene = lastScene st
    if doorsOpen && currentScene == 0 then do
        debug "Garagenlicht: A door opened and scene was off (0), setting scene to dimmed (3)."
        groupWrite sceneGA (DPT18_1 (False, 3))
        modify $ \s -> s { lastScene = 3 }
    else if not (insideDoor st) && not (outsideDoor st) && currentScene == 3 then do
        debug "Garagenlicht: Both doors closed and scene was dimmed (3), setting scene to off (0)."
        groupWrite sceneGA (DPT18_1 (False, 0))
        modify $ \s -> s { lastScene = 0 }
    else
        return ()