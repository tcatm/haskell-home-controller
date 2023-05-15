module TimeSender
    ( TimeSenderConfig (..)
    , timeSender
    ) where

import KNXAddress
import DPTs
import Device

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

data TimeSenderConfig = TimeSenderConfig
  { timeGA :: GroupAddress
  , dateGA :: GroupAddress
  , intervalSeconds :: NominalDiffTime
  } deriving (Show)


timeSender :: TimeSenderConfig -> Device
timeSender conf = Device "TimeSender" (DeviceState) (timeSenderF conf)

timeSenderF :: TimeSenderConfig -> DeviceM (DeviceState) ()
timeSenderF conf = loop
  where
    loop = do
      debug "Sending time"
      time <- zonedTimeToLocalTime <$> getTime
      let timeBytes = timeToBytes time
      let dptTime = DPT10 timeBytes
      groupWrite (timeGA conf) dptTime

      let dateBytes = dateToBytes time
      let dptDate = DPT11 dateBytes
      groupWrite (dateGA conf) dptDate

      scheduleIn (intervalSeconds conf) loop

timeToBytes :: LocalTime -> (Word, Word, Word, Word)
timeToBytes time = (fromIntegral dayOfWeek, hour, minute, second)
  where
    TimeOfDay h m s = localTimeOfDay $ time
    hour = fromIntegral h
    minute = fromIntegral m
    second = floor s
    (year, week, dayOfWeek) = toWeekDate $ localDay time

dateToBytes :: LocalTime -> (Word, Word, Word)
dateToBytes time = (day, month, year)
  where
    (y, m, d) = toGregorian $ localDay $ time
    day = fromIntegral d
    month = fromIntegral m
    year = fromIntegral (y `mod` 100)
