{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DeviceTypes
    ( Continuation (..)
    , Action (..)
    , TimerId (..)
    , DeviceM (..)
    , Device (..)
    , Device' (..)
    ) where

import KNXAddress
import DPTs

import Data.Aeson
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Data
import Data.Binary.Get
import Control.Concurrent
import Data.Map (Map)

data Device = forall s. (Show s, ToJSON s) => Device (Device' s)

data Device' s = Device'    { deviceName :: String
                            , deviceState :: s
                            , deviceContinuations :: [Continuation s]
                            , deviceTimers :: Map TimerId ThreadId
                            } deriving (Show)

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
        in (f a, s'', actions <> actions')

instance Monad (DeviceM s) where
    device >>= f = DeviceM $ \(time, s) ->
        let (a, s', actions) = runDeviceM device (time, s)
            (b, s'', actions') = runDeviceM (f a) (time, s')
        in (b, s'', actions <> actions')

newtype TimerId = TimerId Int deriving (Eq, Ord, Show, Data, Typeable)

instance ToJSON TimerId where
    toJSON (TimerId i) = toJSON i

data Continuation s = StartContinuation (DeviceM s ())
                    | GroupValueContinuation GroupAddress (Get DPT) (DPT -> DeviceM s ())
                    | GroupReadContinuation GroupAddress (DeviceM s ())
                    | ScheduledContinuation TimerId UTCTime (DeviceM s ())

instance Show (Continuation s) where
    show (StartContinuation _) = "StartContinuation"
    show (GroupValueContinuation ga _ _) = "GroupValueContinuation " <> show ga
    show (GroupReadContinuation ga _) = "GroupReadContinuation " <> show ga
    show (ScheduledContinuation timerId time _) = "ScheduledContinuation " <> show timerId <> " " <> show time

data Action s   = GroupWrite GroupAddress DPT
                | GroupResponse GroupAddress DPT
                | GroupRead GroupAddress
                | HueActivateScene String String
                | HueSetRoomOn String Bool
                | Defer (Continuation s)
                | Log String
                | CancelTimer TimerId

instance Show (Action s) where
    show (GroupWrite ga dpt) = "GroupWrite " <> show ga <> " " <> show dpt
    show (GroupResponse ga dpt) = "GroupResponse " <> show ga <> " " <> show dpt
    show (GroupRead ga) = "GroupRead " <> show ga
    show (HueActivateScene room scene) = "HueActivateScene " <> room <> " " <> scene
    show (HueSetRoomOn room on) = "HueSetRoomOn " <> room <> " " <> show on
    show (Defer c) = "Defer " <> show c
    show (Log msg) = "Log " <> msg
    show (CancelTimer timerId) = "CancelTimer " <> show timerId