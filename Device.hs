module Device 
    ( Device (..)
    , Continuation (..)
    , Action (..)
    , DeviceState (..)
    , initialDeviceState
    , modifyState
    , debug
    , groupWrite
    , groupRead
    , scheduleAt
    , scheduleIn
    , getTime
    ) where

import KNXAddress
import DPTs
import Data.Binary.Get
import Data.Time.Clock
import Data.Time.LocalTime

data Continuation = Continuation (Device DeviceState ()) -- Used for starting a device
                  | GroupReadContinuation GroupAddress (Get DPT) (DPT -> Device DeviceState ())
                  | ScheduledContinuation UTCTime (Device DeviceState ())

instance Show Continuation where
    show (Continuation _) = "Continuation"
    show (GroupReadContinuation ga _ _) = "GroupReadContinuation " ++ show ga
    show (ScheduledContinuation time _) = "ScheduledContinuation " ++ show time

data Action = GroupWrite GroupAddress DPT
            | Defer Continuation
            | Log String

instance Show Action where
    show (GroupWrite ga dpt) = "GroupWrite " ++ show ga ++ " " ++ show dpt
    show (Defer c) = "Defer " ++ show c
    show (Log msg) = "Log " ++ msg

data DeviceState = DeviceState 
    { counter :: Int
    } deriving (Show)

initialDeviceState :: DeviceState
initialDeviceState = DeviceState
    { counter = 0
    }

data Device s a = Device { runDevice :: (ZonedTime, DeviceState) -> (a, DeviceState, [Action]) }

instance Show (Device s a) where
    show _ = "Device <function>"

instance Functor (Device s) where
    fmap f device = Device $ \(time, s) -> 
        let (a, s', actions) = runDevice device (time, s)
        in (f a, s', actions)

instance Applicative (Device s) where
    pure a = Device $ \(time, s) -> (a, s, [])

    deviceF <*> deviceA = Device $ \(time, s) ->
        let (f, s', actions) = runDevice deviceF (time, s)
            (a, s'', actions') = runDevice deviceA (time, s')
        in (f a, s'', actions ++ actions')

instance Monad (Device s) where
    device >>= f = Device $ \(time, s) ->
        let (a, s', actions) = runDevice device (time, s)
            (b, s'', actions') = runDevice (f a) (time, s')
        in (b, s'', actions ++ actions')

modifyState :: (DeviceState -> DeviceState) -> Device s ()
modifyState f = Device $ \(time, s) -> ((), f s, [])

debug :: String -> Device s ()
debug msg = Device $ \(time, s) -> ((), s, [Log msg])

groupWrite :: GroupAddress -> DPT -> Device s ()
groupWrite ga dpt = Device $ \(time, s) -> ((), s, [action])
    where
        action = GroupWrite ga dpt

groupRead :: GroupAddress -> (Get DPT) -> (DPT -> Device DeviceState ()) -> Device DeviceState ()
groupRead ga parser cont = Device $ \(time, s) -> ((), s, [Defer $ GroupReadContinuation ga parser cont])

scheduleAt :: UTCTime -> Device DeviceState () -> Device s ()
scheduleAt time device = Device $ \(_, s) -> ((), s, [action])
    where
        action = Defer $ ScheduledContinuation time device

scheduleIn :: NominalDiffTime -> Device DeviceState () -> Device s ()
scheduleIn offset device = do
    now <- getTime
    scheduleAt (addUTCTime offset $ zonedTimeToUTC now) device

getTime :: Device s ZonedTime
getTime = Device $ \(time, s) -> (time, s, [])