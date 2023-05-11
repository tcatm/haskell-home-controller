module Main where

import KNXAddress
import KNX
import Device
import Console
import TimeSender
import Control.Concurrent

knxGatewayHost = "localhost"
knxGatewayPort = "6720"

runWorkerLoop :: KNXConnection -> [Device DeviceState ()] -> MVar IncomingGroupMessage -> IO ()
runWorkerLoop knx devices queue = do
  -- devices with their initial state
  let devicesWithState = map (\device -> (device, initialDeviceState)) devices
  workerLoop knx devicesWithState queue

workerLoop :: KNXConnection -> [(Device DeviceState (), DeviceState)] -> MVar IncomingGroupMessage -> IO ()
workerLoop knx devices queue = do
  msg <- takeMVar queue
  putStrLn $ "Received from KNX: " ++ show msg

  -- process the message for each device, and return the new device state
  -- keep in mind, processDeviceState only returns new state
  -- so we need to combine it with the device to form tuples again
  newDevices <- mapM (\(device, state) -> do
                  newState <- processDeviceState knx msg (device, state)
                  return (device, newState)
                ) devices
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
                , stdinLoop knx
                , runWorkerLoop knx devices $ knxQueue
                ]

  waitAllThreads actions

  disconnectKnx knx
  putStrLn "Closed connection."