{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ElphiWohnung ( config )
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
import GHC.Generics
import Data.Aeson

config = Config
    { devices = [ timeSender timeSenderConfig
                , presenceDevice
                , meanTemperatureWohnzimmer
                , meanTemperatureMasterBedroom
                , switchScene
                , allRoomsLightState
                , rohrbegleitHeizung
                , stoerungen
                , lichtGästeWC
                , lichtGästebad
                , lichtAnkleide
                , vorhangDevice
                , blindsDeviceKitchen
                ]
    }

timeSenderConfig = TimeSenderConfig
    { timeGA = GroupAddress 0 2 10
    , dateGA = GroupAddress 0 2 9
    , intervalSeconds = 300
    }

blindsDeviceKitchen :: Device
blindsDeviceKitchen = makeBlindsDevice "Sonnenschutz Küche" blindsConfigKitchen

blindsConfigKitchen = BlindsConfig
    { upDownGA = GroupAddress 2 2 3
    , stopGA = GroupAddress 2 3 3
    , positionGA = GroupAddress 2 4 3
    , positionStateGA = GroupAddress 2 5 3
    , openGA = GroupAddress 2 1 39
    , closeGA = GroupAddress 2 1 40
    , timeToMove = 19
    , motorStartDelay = 3
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
    let presenceGA = GroupAddress 0 1 12

    respondOnRead presenceGA $ fmap DPT1 <$> gets presence

    watchDPT1 presenceGA $ \presence -> do
        debug $ "Presence: " <> show presence
        case presence of
            True -> enablePresence
            False -> disablePresence

        modify $ \s -> s { presence = Just presence }

enablePresence = do
    groupWrite (GroupAddress 1 0 1) (DPT1 True)     -- Rückmeldung Anwesenheit
    groupWrite (GroupAddress 1 3 1) (DPT5_1 0.7)    -- Bel. Decke Flur 1.1 auf 70%
    groupWrite (GroupAddress 3 4 90) (DPT20_102 KNXHVACModeAuto)    -- Betriebsmodus Wohnung (HVAC) auf Auto
    groupWrite (GroupAddress 3 0 5) (DPT5_1 0.4)    -- Volumenstrom auf 40%
    
    timerId <- gets presenceTimer
    case timerId of
        Just timerId -> do 
            cancelTimer timerId
            modify $ \s -> s { presenceTimer = Nothing }
        Nothing -> return ()

disablePresence = do
    groupWrite (GroupAddress 1 0 1) (DPT1 False)            -- Rückmeldung Anwesenheit
    groupWrite (GroupAddress 0 1 1) (DPT18_1 (False, 0))    -- Szene Aus für ganze Wohnung
    groupWrite (GroupAddress 3 0 5) (DPT5_1 0)              -- Volumenstrom auf 0%
    groupWrite (GroupAddress 1 1 27) (DPT1 False)           -- Steckdose Bett links Master Bedroom aus
    groupWrite (GroupAddress 1 1 28) (DPT1 False)           -- Steckdose Bett rechts Master Bedroom aus
    groupWrite (GroupAddress 1 1 29) (DPT1 False)           -- Handtuch Heizung Masterbad
    groupWrite (GroupAddress 1 1 30) (DPT1 False)           -- Steckdose Gästezimmer
    groupWrite (GroupAddress 1 1 31) (DPT1 False)           -- Handtuch Heizung Gästebad
    groupWrite (GroupAddress 1 1 32) (DPT1 False)           -- Steckdose Bodentank 1
    groupWrite (GroupAddress 1 1 33) (DPT1 False)           -- Steckdose Spiegelheizung Masterbad
   
    let timerDelay = 3 * 24 * 60 * 60
    timerId <- scheduleIn timerDelay $ do
        groupWrite (GroupAddress 3 4 90) (DPT20_102 KNXHVACModeStandby)    -- Betriebsmodus Wohnung (HVAC) auf Standby

    modify $ \s -> s { presenceTimer = Just timerId }

meanTemperatureWohnzimmer :: Device
meanTemperatureWohnzimmer = makeDevice "Mittelwert Temperatur Wohnzimmer" Map.empty $ meanTemperatureDevice addresses $ GroupAddress 3 2 2
    where
        addresses = map (\i -> GroupAddress 3 2 i) [20..24]

meanTemperatureMasterBedroom :: Device
meanTemperatureMasterBedroom = makeDevice "Mittelwert Temperatur Master Bedroom" Map.empty $ meanTemperatureDevice addresses $ GroupAddress 3 2 11
    where
        addresses = map (\i -> GroupAddress 3 2 i) [30..34]

meanTemperatureDevice :: [GroupAddress] -> GroupAddress -> DeviceM (Map.Map GroupAddress Double) ()
meanTemperatureDevice addresses output = do
    forM_ addresses $ \address -> do
        groupRead address
        watchDPT9 address $ \temp -> do
            debug $ "Read " <> show temp <> " from " <> show address
            modify $ Map.insert address temp
            tryAll

    where
        tryAll = do
            temps <- gets $ Map.elems
            case temps of
                [] -> return ()
                _ -> do
                    let meanTemp = sum temps / fromIntegral (length temps)
                    debug $ "Mean temperature: " <> show meanTemp <> "°C, measured temperatures: " <> show temps
                    groupWrite output (DPT9 meanTemp)

switchScene :: Device
switchScene = makeDevice "Lichtschalter Szene Schalten" () $ switchSceneF switchSceneAddresses

switchSceneAddresses = map (\i -> (GroupAddress 0 5 i, GroupAddress 0 1 i)) [1..11]

switchSceneF :: [(GroupAddress, GroupAddress)] -> DeviceM () ()
switchSceneF addresses = do
    forM_ addresses $ \(input, output) -> do
        watchDPT1 input $ \state -> do
            debug $ "Read " <> show state <> " from " <> show input
            let scene = if state then 1 else 0
            groupWrite output (DPT18_1 (False, scene))

data RoomLightState = RoomLightState { roomName :: String, lightStateGAs :: [GroupAddress], lightOutputGA :: GroupAddress }

roomLightStateMap :: [RoomLightState]
roomLightStateMap = [ RoomLightState "Bad" [GroupAddress 1 4 15, GroupAddress 1 4 16] $ GroupAddress 0 4 3
                    , RoomLightState "Gästebad" [GroupAddress 1 4 17] $ GroupAddress 0 4 4
                    , RoomLightState "Flur" [GroupAddress 1 4 1] $ GroupAddress 0 4 2
                    , RoomLightState "Küche" [GroupAddress 1 4 9, GroupAddress 1 4 10] $ GroupAddress 0 4 5
                    , RoomLightState "Loggia" [GroupAddress 1 4 11] $ GroupAddress 0 4 6
                    , RoomLightState "Master Bad"   [ GroupAddress 1 4 12, GroupAddress 1 4 19
                                                    , GroupAddress 1 4 20, GroupAddress 1 4 21
                                                    , GroupAddress 1 4 22
                                                    ] $ GroupAddress 0 4 7
                    , RoomLightState "Master Bedroom"   [ GroupAddress 1 4 7, GroupAddress 1 4 8
                                                        , GroupAddress 1 4 27, GroupAddress 1 4 28
                                                        ] $ GroupAddress 0 4 8
                    , RoomLightState "Schlafzimmer" [GroupAddress 1 4 5, GroupAddress 1 4 6, GroupAddress 1 4 30] $ GroupAddress 0 4 9
                    , RoomLightState "Studio" [GroupAddress 1 4 2, GroupAddress 1 4 3] $ GroupAddress 0 4 10
                    , RoomLightState "Wohnen/Essen" [ GroupAddress 1 4 23, GroupAddress 1 4 25
                                                    -- , GroupAddress 1 4 24, GroupAddress 1 4 26 -- aktuelle keine Lampen angeschlossen
                                                    , GroupAddress 1 4 38, GroupAddress 1 4 39
                                                    ] $ GroupAddress 0 4 11
                    ]

allRoomsLightState :: Device
allRoomsLightState = makeDevice "Lichtstatus alle Räume" Map.empty $ allRoomsLightStateF roomLightStateMap (GroupAddress 0 4 1)

allRoomsLightStateF :: [RoomLightState] -> GroupAddress -> DeviceM (Map.Map String (Map.Map GroupAddress Bool)) ()
allRoomsLightStateF roomLightStateMap outputAllRooms = do
    forM_ roomLightStateMap $ \roomLightState -> do
        forM_ (lightStateGAs roomLightState) $ \ga -> do
            groupRead ga
            watchDPT1 ga $ \state -> do
                debug $ "Read " <> show state <> " from " <> show ga
                states <- gets $ Map.lookup (roomName roomLightState)

                let states' = case states of
                        Nothing -> Map.singleton ga state
                        Just states'' -> Map.insert ga state states''

                modify $ Map.insert (roomName roomLightState) states'

                tryRoom roomLightState
                tryAll

    where
        tryRoom roomLightState = do
            states <- gets $ Map.lookup (roomName roomLightState)
            case states of
                Nothing -> return ()
                Just states' -> do
                    let allStates = Map.elems states'
                    let anyOn = any id allStates
                    debug $ "Room " <> roomName roomLightState <> " is " <> show anyOn <> " (states: " <> show allStates <> ")"
                    groupWrite (lightOutputGA roomLightState) (DPT1 anyOn)
        tryAll = do
            states <- gets $ Map.elems
            case states of
                [] -> return ()
                _ -> do
                    let allStates = concatMap Map.elems states
                    let anyOn = any id allStates
                    debug $ "Any room is " <> show anyOn <> " (states: " <> show allStates <> ")"
                    groupWrite outputAllRooms (DPT1 anyOn)

rohrbegleitHeizung :: Device
rohrbegleitHeizung = makeDevice "Rohrbegleitheizung" Map.empty $ rohrbegleitHeizungF

rohrbegleitHeizungF :: DeviceM (Map.Map GroupAddress Bool) ()
rohrbegleitHeizungF = do
    let addresses = [presenceGA, timerGA, anyLightsOnGA]

    forM_ addresses $ \address -> do
        groupRead address
        watchDPT1 address $ \state -> do
            debug $ "Read " <> show state <> " from " <> show address
            modify $ Map.insert address state
            tryAll

    where
        presenceGA = GroupAddress 1 0 1
        timerGA = GroupAddress 1 0 2
        anyLightsOnGA = GroupAddress 0 4 1

        tryAll = do
            presenceState <- gets $ Map.lookup presenceGA
            timerState <- gets $ Map.lookup timerGA
            anyLightsOnState <- gets $ Map.lookup anyLightsOnGA

            let orGate = case (timerState, anyLightsOnState) of
                    (Just True, _) -> True
                    (_, Just True) -> True
                    _ -> False

            let output = case presenceState of
                    Just True -> orGate
                    _ -> False

            debug $ "Rohrbegleitheizung is " <> show output <> " (presence: " <> show presenceState <> ", timer: " <> show timerState <> ", anyLightsOn: " <> show anyLightsOnState <> ")"
            groupWrite (GroupAddress 1 1 37) (DPT1 output)

data StoerungenState = StoerungenState
    { oredInputs :: Map.Map GroupAddress Bool
    , stoerungDDC :: Bool
    , watchdogTimer :: Maybe TimerId
    } deriving (Show, Generic)

instance ToJSON StoerungenState

stoerungen :: Device
stoerungen = makeDevice "Störungen" (StoerungenState Map.empty False Nothing) $ stoerungenF

stoerungenF :: DeviceM StoerungenState ()
stoerungenF = do
    resetWatchdog

    forM_ watchedGAs $ \address -> do
        groupRead address
        watchDPT1 address $ \state -> do
            debug $ "Read " <> show state <> " from " <> show address
            modify $ \s -> s { oredInputs = Map.insert address state (oredInputs s) }
            updateOutput

    watchDPT1 heartbeatGA $ \state -> do
        debug $ "DDC Heartbeat: " <> show state
        when state $ do
            debug "DDC Heartbeat received"
            modify $ \s -> s { stoerungDDC = False }
            groupWrite stoerungDDCGA (DPT1 False)
            resetWatchdog
            updateOutput

    where
        watchedGAs = map (\i -> GroupAddress 3 5 i) [20..23]
        watchdogTimout = 180
        heartbeatGA = GroupAddress 3 5 4
        stoerungDDCGA = GroupAddress 3 5 19
        sammelstoerungGA = GroupAddress 3 5 41

        resetWatchdog = do
            timerId <- gets $ watchdogTimer
            case timerId of
                Just timerId -> do
                    cancelTimer timerId
                    modify $ \s -> s { watchdogTimer = Nothing }
                Nothing -> return ()

            timerId <- scheduleIn watchdogTimout $ do
                debug "Watchdog triggered"
                modify $ \s -> s { stoerungDDC = True }
                groupWrite stoerungDDCGA (DPT1 True)
                updateOutput

            modify $ \s -> s { watchdogTimer = Just timerId }
        
        updateOutput = do
            inputs <- gets $ oredInputs
            let orGate = any id $ Map.elems inputs 
            ddc <- gets $ stoerungDDC
            let output = orGate || ddc
            debug $ "Störungen is " <> show output <> " (inputs: " <> show inputs <> ", ddc: " <> show ddc <> ")"
            groupWrite sammelstoerungGA (DPT1 output)

lichtGästeWC :: Device
lichtGästeWC = staircaseLight "Treppenhausschaltung Gäste WC" config
    where
        config = StaircaseLightConfig
            { lightOnAddress = GroupAddress 1 4 17
            , lightOffAddress = GroupAddress 1 4 17
            , lightOffTime = 20 * 60
            }

lichtGästebad :: Device
lichtGästebad = staircaseLight "Treppenhausschaltung Gästebad" config
    where
        config = StaircaseLightConfig
            { lightOnAddress = GroupAddress 0 1 3
            , lightOffAddress = GroupAddress 0 1 3
            , lightOffTime = 45 * 60
            }

lichtAnkleide :: Device
lichtAnkleide = staircaseLight "Treppenhausschaltung Ankleide" config
    where
        config = StaircaseLightConfig
            { lightOnAddress = GroupAddress 1 4 4
            , lightOffAddress = GroupAddress 1 4 4
            , lightOffTime = 30 * 60
            }

data Vorhang = Vorhang  { vorhangName :: String
                        , vorhangInputGA :: GroupAddress
                        , vorhangOpenGA :: GroupAddress
                        , vorhangCloseGA :: GroupAddress
                        } deriving (Show)

vorhänge =  [ Vorhang "Sonnenschutz Alle" (GroupAddress 2 0 6) (GroupAddress 2 0 7) (GroupAddress 2 0 8)
            , Vorhang "Sonnenschutz Studio" (GroupAddress 2 1 30) (GroupAddress 2 1 31) (GroupAddress 2 1 32)
            , Vorhang "Sonnenschutz Schlafzimmer" (GroupAddress 2 1 34) (GroupAddress 2 1 35) (GroupAddress 2 1 36)
            , Vorhang "Sonnenschutz Küche" (GroupAddress 2 1 38) (GroupAddress 2 1 39) (GroupAddress 2 1 40)
            , Vorhang "Sonnenschutz Wohnzimmer" (GroupAddress 2 1 42) (GroupAddress 2 1 43) (GroupAddress 2 1 44)
            , Vorhang "Sonnenschutz Master Bedroom" (GroupAddress 2 1 46) (GroupAddress 2 1 47) (GroupAddress 2 1 48)
            , Vorhang "Verdunkelung Alle" (GroupAddress 2 0 2) (GroupAddress 2 0 3) (GroupAddress 2 0 4)
            , Vorhang "Verdunkelung Studio" (GroupAddress 2 1 2) (GroupAddress 2 1 3) (GroupAddress 2 1 4)
            , Vorhang "Verdunkelung Schlafzimmer" (GroupAddress 2 1 6) (GroupAddress 2 1 7) (GroupAddress 2 1 8)
            , Vorhang "Verdunkelung Küche" (GroupAddress 2 1 10) (GroupAddress 2 1 11) (GroupAddress 2 1 12)
            , Vorhang "Verdunkelung Wohnzimmer" (GroupAddress 2 1 14) (GroupAddress 2 1 15) (GroupAddress 2 1 16)
            , Vorhang "Verdunkelung Master Bedroom" (GroupAddress 2 1 18) (GroupAddress 2 1 19) (GroupAddress 2 1 20)
            ]

vorhangDevice :: Device
vorhangDevice = makeDevice "Vorhänge" () $ vorhangDeviceF vorhänge

vorhangDeviceF :: [Vorhang] -> DeviceM () ()
vorhangDeviceF vorhänge = do
    forM_ vorhänge $ \vorhang -> do
        vorhangAufZu (vorhangName vorhang) (vorhangInputGA vorhang) (vorhangOpenGA vorhang) (vorhangCloseGA vorhang)

vorhangAufZu :: String -> GroupAddress -> GroupAddress -> GroupAddress -> DeviceM () ()
vorhangAufZu name inputGA openGA closeGA = do
    watchDPT1 inputGA $ \state -> do
        let stateStr = if state then "zu" else "auf"
        debug $ "Vorhang " <> name <> " " <> show stateStr
        case state of
            -- False means "open", True means "close"
            False -> groupWrite openGA (DPT1 True)
            True -> groupWrite closeGA (DPT1 True)