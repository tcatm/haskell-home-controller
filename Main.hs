module Main where

import KNXAddress
import KNX hiding (groupWrite)
import DPTs
import Device
import DeviceRunner
import Console
import TimeSender
import Control.Concurrent
import Data.Time.Clock
import qualified Data.Map as Map

knxGatewayHost = "localhost"
knxGatewayPort = "6720"

knxCallback :: MVar DeviceInput -> KNXCallback
knxCallback mvar = KNXCallback f
  where
    f msg = putMVar mvar (KNXGroupMessage msg)

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

  deviceInput <- newEmptyMVar

  let devices = [ SomeDevice $ sampleDevice
                , SomeDevice $ timeSender timeSenderConfig
                ]

  let actions = [ runKNX knx $ runKnxLoop (knxCallback deviceInput)
                , stdinLoop knx
                , runKNX knx $ runDevices devices deviceInput
                ]

  waitAllThreads actions

  runKNX knx $ disconnectKnx
  putStrLn "Closed connection."

-- | This device reads two group addresses and prints their sum after both have been read.
-- | When a new value is read from either group address, the sum is recalculated.
sampleDevice :: Device (Map.Map GroupAddress Int)
sampleDevice = Device "Sample Device" Map.empty [Continuation sampleDeviceF]

sampleDeviceF :: DeviceM (Map.Map GroupAddress Int) ()
sampleDeviceF = do
  time <- getTime
  debug $ "Time: " ++ show time
  readAndTry groupAddressA
  readAndTry groupAddressB
  
  where
    groupAddressA = GroupAddress 0 0 1
    groupAddressB = GroupAddress 0 0 2

    readAndTry ga = reader ga tryBoth

    reader ga f = do
      groupRead ga parseDPT6 $ \(DPT6 a) -> do
        debug $ "Read " ++ show a ++ " from " ++ show ga
        modifyState $ Map.insert ga $ fromIntegral a
        f
        reader ga f

    tryBoth = do
      state <- getState
      let a = Map.lookup groupAddressA state
      let b = Map.lookup groupAddressB state
      case (a, b) of
        (Just a', Just b') -> 
          let sum = a' + b'
          in debug $ "a + b = " ++ show sum
        _ -> return ()
    
sceneMultiplexer inputGA offset ouputGA = Device "Scene Multiplexer" () [Continuation (sceneMultiplexerF inputGA offset ouputGA)]

sceneMultiplexerF :: GroupAddress -> Int -> GroupAddress -> DeviceM () ()
sceneMultiplexerF inputAddr offset outputAddr = do
    groupRead inputAddr parseDPT18_1 $ \(DPT18_1 (False, a)) -> do
        groupWrite outputAddr (DPT18_1 (False, a + offset))
        sceneMultiplexerF inputAddr offset outputAddr
