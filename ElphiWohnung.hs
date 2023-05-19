module ElphiWohnung
    ( devices
    )
where

import KNXAddress
import DPTs
import Device

import BlindsDevice
import StaircaseLight
import TimeSender

import qualified Data.Map as Map
import Control.Monad

devices =   [ timeSender timeSenderConfig
            , presenceDevice
            , meanTemperatureWohnzimmer
            , meanTemperatureMasterBedroom
            , switchScene
            , allRoomsLightState
            , blindsDeviceKitchen
            ]

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

presenceDevice :: Device
presenceDevice = makeDevice "Anwesenheit" Nothing presenceDeviceF

presenceDeviceF :: DeviceM (Maybe TimerId) ()
presenceDeviceF = do
    let presenceGA = GroupAddress 0 1 12
    watchDPT1 presenceGA $ \presence -> do
        debug $ "Presence: " ++ show presence
        case presence of
            True -> enablePresence
            False -> disablePresence

enablePresence = do
    groupWrite (GroupAddress 1 0 1) (DPT1 True)     -- Rückmeldung Anwesenheit
    groupWrite (GroupAddress 1 3 1) (DPT5_1 0.7)    -- Bel. Decke Flur 1.1 auf 70%
    groupWrite (GroupAddress 3 4 90) (DPT5 1)       -- Betriebsmodus Wohnung (HVAC) auf Komfort
    groupWrite (GroupAddress 3 0 5) (DPT5_1 0.4)    -- Volumenstrom auf 40%
    
    timerId <- gets id
    case timerId of
        Just timerId -> do 
            cancelTimer timerId
            modify $ const Nothing
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
        groupWrite (GroupAddress 3 4 90) (DPT5 2)  -- Betriebsmodus Wohnung (HVAC) auf Standby

    modify $ const $ Just timerId

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
            debug $ "Read " ++ show temp ++ " from " ++ show address
            modify $ Map.insert address temp
            tryAll

    where
        tryAll = do
            temps <- gets $ Map.elems
            case temps of
                [] -> return ()
                _ -> do
                    let meanTemp = sum temps / fromIntegral (length temps)
                    debug $ "Mean temperature: " ++ show meanTemp ++ "°C, measured temperatures: " ++ show temps
                    groupWrite output (DPT9 meanTemp)

switchScene :: Device
switchScene = makeDevice "Lichtschalter Szene Schalten" () $ switchSceneF switchSceneAddresses

switchSceneAddresses = map (\i -> (GroupAddress 0 5 i, GroupAddress 0 1 i)) [1..11]

switchSceneF :: [(GroupAddress, GroupAddress)] -> DeviceM () ()
switchSceneF addresses = do
    forM_ addresses $ \(input, output) -> do
        watchDPT1 input $ \state -> do
            debug $ "Read " ++ show state ++ " from " ++ show input
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
                debug $ "Read " ++ show state ++ " from " ++ show ga
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
                    debug $ "Room " ++ roomName roomLightState ++ " is " ++ show anyOn ++ " (states: " ++ show allStates ++ ")"
                    groupWrite (lightOutputGA roomLightState) (DPT1 anyOn)
        tryAll = do
            states <- gets $ Map.elems
            case states of
                [] -> return ()
                _ -> do
                    let allStates = concatMap Map.elems states
                    let anyOn = any id allStates
                    debug $ "Any room is " ++ show anyOn ++ " (states: " ++ show allStates ++ ")"
                    groupWrite outputAllRooms (DPT1 anyOn)



-- TODO
-- Rohrbegleitheizung
-- Sonnenschutz alle Räume
-- Verdunkelung alle Räume
-- Treppenhauslicht Ankleide, beide Gästebäder
-- Störmeldungen: Sammelstörung und Watchdog für 3/5/4