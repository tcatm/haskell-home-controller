module Device 
    ( makeDevice
    , gets
    , modify
    , debug
    , groupWrite
    , groupRead
    , onGroupValue
    , onGroupRead
    , groupResponse
    , hueActivateScene
    , hueSetRoomOn
    , scheduleAt
    , scheduleIn
    , cancelTimer
    , getTime
    , eventLoop
    , respondOnRead
    , watchDPT1
    , watchDPT5
    , watchDPT5_1
    , watchDPT9
    , watchDPT18_1
    ) where

import KNXAddress
import KNXDatatypes
import DPTs
import DeviceTypes
import Data.Word
import Data.Binary.Get
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Hashable
import Data.Aeson

makeDevice :: (Show s, ToJSON s) => String -> s -> (DeviceM s ()) -> Device
makeDevice name state device = Device $ Device' name state [StartContinuation device] mempty

gets :: (s -> a) -> DeviceM s a
gets f = DeviceM $ \(time, s) -> (f s, s, [])

modify :: (s -> s) -> DeviceM s ()
modify f = DeviceM $ \(time, s) -> ((), f s, [])

debug :: String -> DeviceM s ()
debug msg = DeviceM $ \(time, s) -> ((), s, [Log msg])

action :: Action s -> DeviceM s ()
action a = DeviceM $ \(time, s) -> ((), s, [a])

-- | Trigger a group read. Use before groupValue to request a value from the bus instead of waiting for it to be sent.
groupRead :: GroupAddress -> DeviceM s ()
groupRead ga = action $ GroupRead ga

-- | Write a group value to the bus.
groupWrite :: GroupAddress -> DPT -> DeviceM s ()
groupWrite ga dpt = action $ GroupWrite ga dpt

-- | Respond to a group value request.
groupResponse :: GroupAddress -> DPT -> DeviceM s ()
groupResponse ga dpt = action $ GroupResponse ga dpt

-- | Wait for a group value to be received. The parser is used to parse the value from the bus.
onGroupValue :: GroupAddress -> (Get DPT) -> (DPT -> DeviceM s ()) -> DeviceM s ()
onGroupValue ga parser cont = action $ Defer $ GroupValueContinuation ga parser cont

-- | Wait for a group read request to be received.
onGroupRead :: GroupAddress -> DeviceM s () -> DeviceM s ()
onGroupRead ga cont = action $ Defer $ GroupReadContinuation ga cont

-- | Activate a Hue scene.
hueActivateScene :: String -> String -> DeviceM s ()
hueActivateScene room scene = action $ HueActivateScene room scene

-- | Turn a Hue room on or off.
hueSetRoomOn :: String -> Bool -> DeviceM s ()
hueSetRoomOn room on = action $ HueSetRoomOn room on

-- | Schedule an action to be run at a specific time.
scheduleAt :: UTCTime -> DeviceM s () -> DeviceM s (TimerId)
scheduleAt time device = DeviceM $ \(_, s) -> (timerId, s, [action])
    where
        action = Defer $ ScheduledContinuation timerId time device
        timerId = TimerId $ hash $ show time

-- | Schedule an action to be run after a duration given in seconds.
scheduleIn :: NominalDiffTime -> DeviceM s () -> DeviceM s (TimerId)
scheduleIn offset device = do
    now <- getTime
    scheduleAt (addUTCTime offset $ zonedTimeToUTC now) device

-- | Cancel a scheduled action.
cancelTimer :: TimerId -> DeviceM s ()
cancelTimer timerId = DeviceM $ \(time, s) -> ((), s, [CancelTimer timerId])

-- | Get the current time.
getTime :: DeviceM s ZonedTime
getTime = DeviceM $ \(time, s) -> (time, s, [])

--- Library functions

eventLoop :: ((a -> DeviceM s ()) -> DeviceM s ()) -> (a -> DeviceM s ()) -> DeviceM s ()
eventLoop f h = f $ \a -> do
    h a
    eventLoop f h

eventLoop' :: (DeviceM s () -> DeviceM s ()) -> DeviceM s () -> DeviceM s ()
eventLoop' f h = f $ do
    h
    eventLoop' f h

-- | Respond to a group read request with a value from a handler.
respondOnRead :: GroupAddress -> DeviceM s (Maybe DPT) -> DeviceM s ()
respondOnRead ga handler = eventLoop' (onGroupRead ga) $ do
    value <- handler
    case value of
        Just value -> groupResponse ga value
        Nothing -> return ()

watchDPT1 :: GroupAddress -> (Bool -> DeviceM s ()) -> DeviceM s ()
watchDPT1 ga handler = eventLoop (onGroupValue ga getDPT1) $ \(DPT1 val) -> handler val

watchDPT5 :: GroupAddress -> (Word8 -> DeviceM s ()) -> DeviceM s ()
watchDPT5 ga handler = eventLoop (onGroupValue ga getDPT5) $ \(DPT5 val) -> handler val

watchDPT5_1 :: GroupAddress -> (Double -> DeviceM s ()) -> DeviceM s ()
watchDPT5_1 ga handler = eventLoop (onGroupValue ga getDPT5_1) $ \(DPT5_1 val) -> handler val

watchDPT9 :: GroupAddress -> (Double -> DeviceM s ()) -> DeviceM s ()
watchDPT9 ga handler = eventLoop (onGroupValue ga getDPT9) $ \(DPT9 val) -> handler val

watchDPT18_1 :: GroupAddress -> ((Bool, Int) -> DeviceM s ()) -> DeviceM s ()
watchDPT18_1 ga handler = eventLoop (onGroupValue ga getDPT18_1) $ \(DPT18_1 (val1, val2)) -> handler (val1, val2)

watchDPT20_102 :: GroupAddress -> (KNXHVACMode -> DeviceM s ()) -> DeviceM s ()
watchDPT20_102 ga handler = eventLoop (onGroupValue ga getDPT20_102) $ \(DPT20_102 val) -> handler val