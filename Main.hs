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

  let devices = [sampleDevice]

  let actions = [ runKNX knx $ runKnxLoop (knxCallback deviceInput)
                , timeSender timeSenderConfig knx
                , stdinLoop knx
                , runDevicesLoop knx devices $ deviceInput
                ]

  waitAllThreads actions

  runKNX knx $ disconnectKnx
  putStrLn "Closed connection."


sampleDevice :: Device (DeviceState) ()
sampleDevice = do
    time <- getTime

    modifyState $ \(DeviceState counter) -> DeviceState (counter + 1)

    debug $ "Time: " ++ show time
    groupRead (GroupAddress 0 1 21) parseDPT18_1 $ \(DPT18_1 (False, a)) -> do
        debug $ "a: " ++ show a
        modifyState $ \(DeviceState counter) -> DeviceState (counter + 1)
        time <- getTime
        debug $ "Time: " ++ show time
        schedule (addUTCTime 5 time) $ do
            modifyState $ \(DeviceState counter) -> DeviceState (counter + 1)
            debug $ "a: " ++ show a
            groupWrite (GroupAddress 0 1 11) (DPT18_1 (False, a))

        sampleDevice
