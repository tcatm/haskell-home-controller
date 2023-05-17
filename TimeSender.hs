module TimeSender
    ( TimeSenderConfig (..)
    , timeSender
    ) where

import KNXAddress
import DPTs
import Device

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
timeSenderF conf = loop
  where
    loop = do
      debug "Sending time"
      time <- zonedTimeToLocalTime <$> getTime
      let day = localDay time
      let dptTime = DPT10 $ KNXTimeOfDay (Just $ dayOfWeek day) (localTimeOfDay time)
      groupWrite (timeGA conf) dptTime

      let dptDate = DPT11 day
      groupWrite (dateGA conf) dptDate

      scheduleIn (intervalSeconds conf) loop

      return ()