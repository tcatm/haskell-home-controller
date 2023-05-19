module Main where

import KNXAddress
import KNX hiding (groupWrite, groupRead)
import DPTs
import Device
import DeviceRunner
import Console
import Webinterface

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as Map

import StaircaseLight
import BlindsDevice
import qualified ElphiWohnung as Elphi

knxGatewayHost = "localhost"
knxGatewayPort = "6720"

knxCallback :: TChan DeviceInput -> KNXCallback
knxCallback chan = KNXCallback $ atomically <$> writeTChan chan . KNXIncomingMessage

-- Define a helper function to create a thread and return an MVar
forkIOWithSync :: LoggingT IO () -> LoggingT IO (MVar ())
forkIOWithSync action = do
    logger <- askLoggerIO
    syncVar <- liftIO newEmptyMVar
    liftIO $ forkIO $ do
        runLoggingT action logger
        putMVar syncVar ()
    return syncVar

-- Define the main function to create and synchronize threads for a list of actions
waitAllThreads :: [LoggingT IO ()] -> LoggingT IO ()
waitAllThreads actions = do
  syncVars <- mapM forkIOWithSync actions
  liftIO $ mapM_ takeMVar syncVars

logFilter :: LogSource -> LogLevel -> Bool
logFilter logSourceKNX LevelDebug = False
logFilter _ _ = True

main :: IO ()
main = runStdoutLoggingT $ filterLogger logFilter $ do
  deviceInput <- liftIO $ newTChanIO
  knxContext <- createKNXContext knxGatewayHost knxGatewayPort (knxCallback deviceInput)
  
  let devices = Elphi.devices

  let actions = [ runKnx knxContext
                , stdinLoop (sendQueue knxContext)
                , runDevices devices deviceInput (sendQueue knxContext)
                , runWebinterface
                ]

  waitAllThreads actions

-- | This device reads two group addresses and prints their sum after both have been read.
-- | When a new value is read from either group address, the sum is recalculated.
sampleDevice :: Device
sampleDevice = makeDevice "Sample Device" Map.empty sampleDeviceF

sampleDeviceF :: DeviceM (Map.Map GroupAddress Int) ()
sampleDeviceF = do
  time <- getTime
  debug $ "Time: " <> show time
  readAndTry groupAddressA
  readAndTry groupAddressB
  
  where
    groupAddressA = GroupAddress 0 0 1
    groupAddressB = GroupAddress 0 0 2

    readAndTry ga = eventLoop (onGroupValue ga getDPT6) $ \(DPT6 a) -> do
        debug $ "Read " <> show a <> " from " <> show ga
        modify $ Map.insert ga $ fromIntegral a
        tryBoth

    tryBoth = do
      a <- gets $ Map.lookup groupAddressA
      b <- gets $ Map.lookup groupAddressB
      case (a, b) of
        (Just a', Just b') -> 
          let sum = a' + b'
          in debug $ "a + b = " <> show sum
        _ -> return ()
    
sceneMultiplexer inputGA offset ouputGA = makeDevice "Scene Multiplexer" () $ sceneMultiplexerF inputGA offset ouputGA

sceneMultiplexerF :: GroupAddress -> Int -> GroupAddress -> DeviceM () ()
sceneMultiplexerF inputAddr offset outputAddr = do
    watchDPT18_1 inputAddr $ \(_, a) ->
        groupWrite outputAddr (DPT18_1 (False, a + offset))
