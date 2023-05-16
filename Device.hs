{-# LANGUAGE ExistentialQuantification #-}

module Device 
    ( DeviceM (..)
    , Device (..)
    , SomeDevice (..)
    , Continuation (..)
    , Action (..)
    , TimerId (..)
    , makeDevice
    , getState
    , setState
    , gets
    , modify
    , debug
    , groupWrite
    , groupRead
    , scheduleAt
    , scheduleIn
    , cancelTimer
    , getTime
    ) where

import KNXAddress
import DPTs
import Data.Binary.Get
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Hashable
import Data.Map (Map)
import Control.Concurrent

data SomeDevice = forall s. Show s => SomeDevice (Device s)

data Device s = Device  { deviceName :: String
                        , deviceState :: s
                        , deviceContinuations :: [Continuation s]
                        , deviceTimers :: Map TimerId ThreadId
                        } deriving (Show)

newtype TimerId = TimerId Int deriving (Eq, Ord, Show)

data Continuation s = Continuation (DeviceM s ()) -- Used for starting a device
                    | GroupReadContinuation GroupAddress (Get DPT) (DPT -> DeviceM s ())
                    | ScheduledContinuation TimerId UTCTime (DeviceM s ())

instance Show (Continuation s) where
    show (Continuation _) = "Continuation"
    show (GroupReadContinuation ga _ _) = "GroupReadContinuation " ++ show ga
    show (ScheduledContinuation timerId time _) = "ScheduledContinuation " ++ show timerId ++ " " ++ show time

data Action s   = GroupWrite GroupAddress DPT
                | Defer (Continuation s)
                | Log String
                | CancelTimer TimerId

instance Show (Action s) where
    show (GroupWrite ga dpt) = "GroupWrite " ++ show ga ++ " " ++ show dpt
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

makeDevice :: (Show s) => String -> s -> (DeviceM s ()) -> SomeDevice
makeDevice name state device = SomeDevice $ Device name state [Continuation device] mempty

getState :: DeviceM s s
getState = DeviceM $ \(time, s) -> (s, s, [])

setState :: s -> DeviceM s ()
setState s = DeviceM $ \(time, _) -> ((), s, [])

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

groupRead :: GroupAddress -> (Get DPT) -> (DPT -> DeviceM s ()) -> DeviceM s ()
groupRead ga parser cont = DeviceM $ \(time, s) -> ((), s, [Defer $ GroupReadContinuation ga parser cont])

scheduleAt :: UTCTime -> DeviceM s () -> DeviceM s (TimerId)
scheduleAt time device = DeviceM $ \(_, s) -> (timerId, s, [action])
    where
        action = Defer $ ScheduledContinuation timerId time device
        timerId = TimerId $ hash (show time ++ show device)

scheduleIn :: NominalDiffTime -> DeviceM s () -> DeviceM s (TimerId)
scheduleIn offset device = do
    now <- getTime
    scheduleAt (addUTCTime offset $ zonedTimeToUTC now) device

cancelTimer :: TimerId -> DeviceM s ()
cancelTimer timerId = DeviceM $ \(time, s) -> ((), s, [CancelTimer timerId])

getTime :: DeviceM s ZonedTime
getTime = DeviceM $ \(time, s) -> (time, s, [])