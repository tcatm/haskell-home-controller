module TimeSender
    ( TimeSenderConfig (..)
    , timeSender
    ) where

import KNX
import KNXAddress
import DPTs

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

import Control.Concurrent
import Control.Monad

data TimeSenderConfig = TimeSenderConfig
  { timeGA :: GroupAddress
  , dateGA :: GroupAddress
  , intervalSeconds :: Int
  } deriving (Show)

timeSender :: TimeSenderConfig -> KNXConnection -> IO ()
timeSender conf knx = do
  putStrLn "Starting time sender thread"
  forever $ do
    putStrLn "Sending time"
    time <- zonedTimeToLocalTime <$> getZonedTime
    let timeBytes = timeToBytes time
    let dptTime = DPT10 timeBytes
    groupWrite knx (timeGA conf) dptTime 

    let dateBytes = dateToBytes time
    let dptDate = DPT11 dateBytes
    groupWrite knx (dateGA conf) dptDate
    threadDelay $ 1000000 * (intervalSeconds conf)

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
