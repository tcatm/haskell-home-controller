module Main where

import System.IO
import Data.Binary
import Data.Binary.Get
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
import Data.Maybe

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
      groupWrite knx $ GroupMessage groupAddress dpt
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
              "DPT18_1" -> DPT18_1 $ (False, read $ head value)

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

runWorkerLoop :: KNXConnection -> [Device Int ()] -> MVar IncomingGroupMessage -> IO ()
runWorkerLoop knx devices queue = do
  -- devices with their initial state
  let devicesWithState = map (\device -> (device, 0)) devices
  workerLoop knx devicesWithState queue

workerLoop :: KNXConnection -> [(Device Int (), Int)] -> MVar IncomingGroupMessage -> IO ()
workerLoop knx devices queue = do
  msg <- takeMVar queue
  putStrLn $ "Received from KNX: " ++ show msg

  newDevices <- mapM (processDeviceState knx msg) devices
  workerLoop knx newDevices queue

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

  knxQueue <- newEmptyMVar

  let devices = [sampleDevice]

  let actions = [ runKnxLoop knx knxQueue
                , timeSender timeSenderConfig knx
                , stdinLoop stdin knx
                , runWorkerLoop knx devices $ knxQueue
                ]

  waitAllThreads actions

  disconnectKnx knx
  putStrLn "Closed connection."

data Device s a = Device { runDevice :: s -> IncomingGroupMessage -> (a, s, [GroupMessage]) }

instance Functor (Device s) where
    fmap f device = Device $ \s msg -> 
        let (a, s', msgs) = runDevice device s msg
        in (f a, s', msgs)

instance Applicative (Device s) where
    pure a = Device $ \s _ -> (a, s, [])
    deviceF <*> deviceA = Device $ \s msg ->
        let (f, s', msgsF) = runDevice deviceF s msg
            (a, s'', msgsA) = runDevice deviceA s' msg
        in (f a, s'', msgsF ++ msgsA)

instance Monad (Device s) where
    device >>= f = Device $ \s msg ->
        let (a, s', msgs) = runDevice device s msg
            Device g = f a
            (a', s'', msgs') = g s' msg
        in (a', s'', msgs ++ msgs')

getState :: Device s s
getState = Device $ \s _ -> (s, s, [])

modifyState :: (s -> s) -> Device s ()
modifyState f = Device $ \s _ -> ((), f s, [])

sendMessage :: GroupMessage -> Device s ()
sendMessage msg = Device $ \s _ -> ((), s, [msg])

getInputMessage :: Device s IncomingGroupMessage
getInputMessage = Device $ \s msg -> (msg, s, [])

processDeviceState :: KNXConnection -> IncomingGroupMessage -> (Device Int (), Int) -> IO (Device Int (), Int)
processDeviceState knx msg (device, state) = do
  let (_, newState, outputMessages) = runDevice device state msg
  performDeviceActions knx outputMessages
  when (newState /= state) $ putStrLn $ "New state: " ++ show newState
  return (device, newState)

performDeviceActions :: KNXConnection -> [GroupMessage] -> IO ()
performDeviceActions knx outputMessages = do
    mapM_ (\outputMessage -> do
          putStrLn $ "Output message: " ++ show outputMessage
          -- Send the output messages to the KNX bus
          groupWrite knx outputMessage
          ) outputMessages

sampleDevice :: Device Int ()
sampleDevice = do
    msg <- getInputMessage
    case msg of
      IncomingGroupMessage (GroupAddress 0 1 11) payload -> do
        let EncodedDPT bs short = payload
        let dpt = runGet parseDPT18_1 bs
        modifyState (+1)
        sendMessage $ GroupMessage (GroupAddress 0 1 12) dpt
      _ -> return ()
