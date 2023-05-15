module Device 
    ( DeviceM (..)
    , Device (..)
    , Continuation (..)
    , Action (..)
    , DeviceState (..)
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

data Device = Device { deviceName :: String
                     , deviceState :: DeviceState
                     , deviceContinuations :: [Continuation]
                     }

data Continuation = Continuation (DeviceM DeviceState ()) -- Used for starting a device
                  | GroupReadContinuation GroupAddress (Get DPT) (DPT -> DeviceM DeviceState ())
                  | ScheduledContinuation UTCTime (DeviceM DeviceState ())

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

data DeviceM s a = DeviceM { runDeviceM :: (ZonedTime, DeviceState) -> (a, DeviceState, [Action]) }

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

data DeviceState = DeviceState 
    { 
    } deriving (Show)

modifyState :: (DeviceState -> DeviceState) -> DeviceM s ()
modifyState f = DeviceM $ \(time, s) -> ((), f s, [])

debug :: String -> DeviceM s ()
debug msg = DeviceM $ \(time, s) -> ((), s, [Log msg])

groupWrite :: GroupAddress -> DPT -> DeviceM s ()
groupWrite ga dpt = DeviceM $ \(time, s) -> ((), s, [action])
    where
        action = GroupWrite ga dpt

groupRead :: GroupAddress -> (Get DPT) -> (DPT -> DeviceM DeviceState ()) -> DeviceM DeviceState ()
groupRead ga parser cont = DeviceM $ \(time, s) -> ((), s, [Defer $ GroupReadContinuation ga parser cont])

scheduleAt :: UTCTime -> DeviceM DeviceState () -> DeviceM s ()
scheduleAt time device = DeviceM $ \(_, s) -> ((), s, [action])
    where
        action = Defer $ ScheduledContinuation time device

scheduleIn :: NominalDiffTime -> DeviceM DeviceState () -> DeviceM s ()
scheduleIn offset device = do
    now <- getTime
    scheduleAt (addUTCTime offset $ zonedTimeToUTC now) device

getTime :: DeviceM s ZonedTime
getTime = DeviceM $ \(time, s) -> (time, s, [])