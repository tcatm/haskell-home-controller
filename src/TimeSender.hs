module TimeSender
    ( TimeSenderConfig (..)
    , timeSender
    ) where

import KNXAddress
import DPTs
import Device
import DeviceTypes

import KNXDatatypes

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar

data TimeSenderConfig = TimeSenderConfig
  { timeGA :: GroupAddress
  , dateGA :: GroupAddress
  , intervalSeconds :: NominalDiffTime
  } deriving (Show)


timeSender :: TimeSenderConfig -> Device
timeSender conf = makeDevice "TimeSender" () (timeSenderF conf)

timeSenderF :: TimeSenderConfig -> DeviceM () ()
timeSenderF conf = do
  respondOnRead (timeGA conf) $ do
    debug "Time read"
    time <- zonedTimeToLocalTime <$> getTime
    Just <$> getTimeDpt time

  respondOnRead (dateGA conf) $ do
    debug "Date read"
    time <- zonedTimeToLocalTime <$> getTime
    Just <$> getDateDpt time

  loop

  where
    loop = do
      debug "Sending time"
      time <- zonedTimeToLocalTime <$> getTime
      getTimeDpt time >>= groupWrite (timeGA conf)
      getDateDpt time >>= groupWrite (dateGA conf)

      scheduleIn (intervalSeconds conf) loop

      return ()

    getTimeDpt time = do
      let day = localDay time
      let dptTime = DPT10 $ KNXTimeOfDay (Just $ dayOfWeek day) (localTimeOfDay time)
      return dptTime

    getDateDpt time = do
      let day = localDay time
      let dptDate = DPT11 day
      return dptDate