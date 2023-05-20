{-# LANGUAGE ExistentialQuantification #-}

module Device 
    ( DeviceM (..)
    , Device (..)
    , Device' (..)
    , Continuation (..)
    , Action (..)
    , TimerId (..)
    , makeDevice
    , gets
    , modify
    , debug
    , groupWrite
    , groupRead
    , groupValue
    , scheduleAt
    , scheduleIn
    , cancelTimer
    , getTime
    , eventLoop
    , watchDPT1
    , watchDPT5
    , watchDPT5_1
    , watchDPT9
    , watchDPT18_1
    ) where

import KNXAddress
import KNXDatatypes
import DPTs
import Data.Word
import Data.Binary.Get
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Hashable
import Data.Map (Map)
import Control.Concurrent

data Device = forall s. Show s => Device (Device' s)

data Device' s = Device'    { deviceName :: String
                            , deviceState :: s
                            , deviceContinuations :: [Continuation s]
                            , deviceTimers :: Map TimerId ThreadId
                            } deriving (Show)

newtype TimerId = TimerId Int deriving (Eq, Ord, Show)

data Continuation s = StartContinuation (DeviceM s ())
                    | GroupValueContinuation GroupAddress (Get DPT) (DPT -> DeviceM s ())
                    | ScheduledContinuation TimerId UTCTime (DeviceM s ())

instance Show (Continuation s) where
    show (StartContinuation _) = "StartContinuation"
    show (GroupValueContinuation ga _ _) = "GroupValueContinuation " ++ show ga
    show (ScheduledContinuation timerId time _) = "ScheduledContinuation " ++ show timerId ++ " " ++ show time

data Action s   = GroupWrite GroupAddress DPT
                | GroupRead GroupAddress
                | Defer (Continuation s)
                | Log String
                | CancelTimer TimerId

instance Show (Action s) where
    show (GroupWrite ga dpt) = "GroupWrite " ++ show ga ++ " " ++ show dpt
    show (GroupRead ga) = "GroupRead " ++ show ga
    show (Defer c) = "Defer " ++ show c
    show (Log msg) = "Log " ++ msg
    show (CancelTimer timerId) = "CancelTimer " ++ show timerId

data DeviceM s a = DeviceM { runDeviceM :: (ZonedTime, s) -> (a, s, [Action s]) }

instance Show (DeviceM s a) where
    show _ = "Device <function>"

instance Functor (DeviceM s) where
    fmap f device = DeviceM $ \(time, s) -> 
        let (a, s', actions) = runDeviceM device (time, s)
        in (f a, s', actions)

instance Applicative (DeviceM s) where
    pure a = DeviceM $ \(time, s) -> (a, s, [])

    deviceF <*> deviceA = DeviceM $ \(time, s) ->
        let (f, s', actions) = runDeviceM deviceF (time, s)
            (a, s'', actions') = runDeviceM deviceA (time, s')
        in (f a, s'', actions ++ actions')

instance Monad (DeviceM s) where
    device >>= f = DeviceM $ \(time, s) ->
        let (a, s', actions) = runDeviceM device (time, s)
            (b, s'', actions') = runDeviceM (f a) (time, s')
        in (b, s'', actions ++ actions')

makeDevice :: (Show s) => String -> s -> (DeviceM s ()) -> Device
makeDevice name state device = Device $ Device' name state [StartContinuation device] mempty

gets :: (s -> a) -> DeviceM s a
gets f = DeviceM $ \(time, s) -> (f s, s, [])

modify :: (s -> s) -> DeviceM s ()
modify f = DeviceM $ \(time, s) -> ((), f s, [])

debug :: String -> DeviceM s ()
debug msg = DeviceM $ \(time, s) -> ((), s, [Log msg])

groupWrite :: GroupAddress -> DPT -> DeviceM s ()
groupWrite ga dpt = DeviceM $ \(time, s) -> ((), s, [action])
    where
        action = GroupWrite ga dpt

action :: Action s -> DeviceM s ()
action a = DeviceM $ \(time, s) -> ((), s, [a])

-- | Wait for a group value to be received. The parser is used to parse the value from the bus.
groupValue :: GroupAddress -> (Get DPT) -> (DPT -> DeviceM s ()) -> DeviceM s ()
groupValue ga parser cont = action $ Defer $ GroupValueContinuation ga parser cont

-- | Trigger a group read. Use before groupValue to request a value from the bus instead of waiting for it to be sent.
groupRead :: GroupAddress -> DeviceM s ()
groupRead ga = action $ GroupRead ga

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

watchDPT1 :: GroupAddress -> (Bool -> DeviceM s ()) -> DeviceM s ()
watchDPT1 ga handler = eventLoop (groupValue ga getDPT1) $ \(DPT1 val) -> handler val

watchDPT5 :: GroupAddress -> (Word8 -> DeviceM s ()) -> DeviceM s ()
watchDPT5 ga handler = eventLoop (groupValue ga getDPT5) $ \(DPT5 val) -> handler val

watchDPT5_1 :: GroupAddress -> (Double -> DeviceM s ()) -> DeviceM s ()
watchDPT5_1 ga handler = eventLoop (groupValue ga getDPT5_1) $ \(DPT5_1 val) -> handler val

watchDPT9 :: GroupAddress -> (Double -> DeviceM s ()) -> DeviceM s ()
watchDPT9 ga handler = eventLoop (groupValue ga getDPT9) $ \(DPT9 val) -> handler val

watchDPT18_1 :: GroupAddress -> ((Bool, Int) -> DeviceM s ()) -> DeviceM s ()
watchDPT18_1 ga handler = eventLoop (groupValue ga getDPT18_1) $ \(DPT18_1 (val1, val2)) -> handler (val1, val2)

watchDPT20_102 :: GroupAddress -> (KNXHVACMode -> DeviceM s ()) -> DeviceM s ()
watchDPT20_102 ga handler = eventLoop (groupValue ga getDPT20_102) $ \(DPT20_102 val) -> handler val