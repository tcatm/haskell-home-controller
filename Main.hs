module Main where

import System.IO
import Data.Binary
import Data.ByteString.Lazy (unpack, pack)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Concurrent (forkIO)
import KNXAddress
import APDU
import KNX
import DPTs
import TimeSender
import Data.Time.Clock (UTCTime, getCurrentTime, utctDayTime, utctDay)
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Calendar
import Data.Time
import Data.Time.Calendar.WeekDate
import Control.Concurrent
import Control.Monad

knxGatewayHost = "localhost"
knxGatewayPort = "6720"

stdinLoop :: Handle -> KNXConnection -> IO ()
stdinLoop handle knx = 
  forever $ do
  line <- hGetLine handle
  putStrLn $ "Received from stdin: " ++ line
  -- Parse "1/2/3 0 0 0 ..." to KNXAdress + List of integers
  let parts = words line
  case parseInput parts of
    Just (groupAddress, dpt) -> do
      -- Do something with the parsed values
      putStrLn $ "Parsed: " ++ show groupAddress ++ " " ++ show dpt
      groupWrite knx groupAddress dpt
      return ()
    Nothing -> putStrLn "Failed to parse input. Format should be: main/middle/sub byte1 byte2 byte3 ..."

-- Parse a String like: 0/1/2 DPT1 True
parseInput :: [String] -> Maybe (GroupAddress, DPT)
parseInput (groupAddressStr:dptName:value) = do
  groupAddress <- parseGroupAddressStr groupAddressStr
  let dpt = case dptName of
    -- read and parse to bool
              "DPT1" -> DPT1 $ readBool value
              "DPT2" -> DPT2 $ readBoolTuple value
              "DPT3" -> DPT3 $ read $ head value
              "DPT4" -> DPT4 $ read $ head value
              "DPT5" -> DPT5 $ read $ head value
              "DPT6" -> DPT6 $ read $ head value
              "DPT7" -> DPT7 $ read $ head value
              "DPT8" -> DPT8 $ read $ head value
              "DPT9" -> DPT9 $ read $ head value
              "DPT10" -> DPT10 $ (\[a, b, c, d] -> (a, b, c, d)) $ map read value
              "DPT11" -> DPT11 $ (\[a, b, c] -> (a, b, c)) $ map read value
              "DPT12" -> DPT12 $ read $ head value
              "DPT13" -> DPT13 $ read $ head value
              "DPT14" -> DPT14 $ read $ head value
              "DPT15" -> DPT15 $ read $ head value
              "DPT16" -> DPT16 $ head value

  return (groupAddress, dpt)

  where
    readBool :: [String] -> Bool
    readBool ["True"] = True
    readBool ["False"] = False
    readBool _ = error "Failed to parse bool"

    readBoolTuple :: [String] -> (Bool, Bool)
    readBoolTuple ["True", "True"] = (True, True)
    readBoolTuple ["True", "False"] = (True, False)
    readBoolTuple ["False", "True"] = (False, True)
    readBoolTuple ["False", "False"] = (False, False)
    readBoolTuple _ = error "Failed to parse bool tuple"
parseInput _ = Nothing

-- Define a helper function to create a thread and return an MVar
forkIOWithSync :: IO () -> IO (MVar ())
forkIOWithSync action = do
  syncVar <- newEmptyMVar
  forkIO $ action >> putMVar syncVar ()
  return syncVar

-- Define the main function to create and synchronize threads for a list of actions
waitAllThreads :: [IO ()] -> IO ()
waitAllThreads actions = do
  syncVars <- mapM forkIOWithSync actions
  sequence_ (map takeMVar syncVars)

main :: IO ()
main = do
  knx <- connectKnx knxGatewayHost knxGatewayPort
  putStrLn "Connected to KNX gateway."

  let timeSenderConfig = TimeSenderConfig {
    timeGA = GroupAddress 0 0 1,
    dateGA = GroupAddress 0 0 2,
    intervalSeconds = 10
  }

  let actions = [runKnxLoop knx, timeSender timeSenderConfig knx, stdinLoop stdin knx]

  waitAllThreads actions

  disconnectKnx knx
  putStrLn "Closed connection."