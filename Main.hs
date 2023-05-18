module Main where

import KNXAddress
import KNX (connectKnx, KNXCallback(..), runKNX, runKnxLoop, disconnectKnx)
import DPTs
import Device
import DeviceRunner
import Console
import TimeSender
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as Map

import StaircaseLight
import BlindsDevice

knxGatewayHost = "localhost"
knxGatewayPort = "6720"

knxCallback :: TQueue DeviceInput -> KNXCallback
knxCallback queue = KNXCallback $ atomically <$> writeTQueue queue . KNXIncomingMessage

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
  knx <- connectKnx knxGatewayHost knxGatewayPort
  deviceInput <- liftIO $ newTQueueIO

  let devices = [ sampleDevice
                --, timeSender timeSenderConfig
                --, staircaseLight
                , blindsDeviceKitchen
                ] ++ temperatureLoggers

  let actions = [ runKNX knx $ runKnxLoop (knxCallback deviceInput)
                , stdinLoop knx
                , runKNX knx $ runDevices devices deviceInput
                ]

  waitAllThreads actions

timeSenderConfig = TimeSenderConfig
                            { timeGA = GroupAddress 0 0 1
                            , dateGA = GroupAddress 0 0 2
                            , intervalSeconds = 10
                            }

temperatureGAs = map (\i -> GroupAddress 3 2 i) [1..12]
temperatureLoggers = map temperatureLogger temperatureGAs

-- | This device reads two group addresses and prints their sum after both have been read.
-- | When a new value is read from either group address, the sum is recalculated.
sampleDevice :: Device
sampleDevice = makeDevice "Sample Device" Map.empty sampleDeviceF

sampleDeviceF :: DeviceM (Map.Map GroupAddress Int) ()
sampleDeviceF = do
  time <- getTime
  debug $ "Time: " ++ show time
  readAndTry groupAddressA
  readAndTry groupAddressB
  
  where
    groupAddressA = GroupAddress 0 0 1
    groupAddressB = GroupAddress 0 0 2

    readAndTry ga = eventLoop (groupValue ga parseDPT6) $ \(DPT6 a) -> do
        debug $ "Read " ++ show a ++ " from " ++ show ga
        modify $ Map.insert ga $ fromIntegral a
        tryBoth

    tryBoth = do
      a <- gets $ Map.lookup groupAddressA
      b <- gets $ Map.lookup groupAddressB
      case (a, b) of
        (Just a', Just b') -> 
          let sum = a' + b'
          in debug $ "a + b = " ++ show sum
        _ -> return ()
    
sceneMultiplexer inputGA offset ouputGA = makeDevice "Scene Multiplexer" () $ sceneMultiplexerF inputGA offset ouputGA

sceneMultiplexerF :: GroupAddress -> Int -> GroupAddress -> DeviceM () ()
sceneMultiplexerF inputAddr offset outputAddr = do
    eventLoop (groupValue inputAddr parseDPT18_1) $ \(DPT18_1 (False, a)) -> do
        groupWrite outputAddr (DPT18_1 (False, a + offset))

temperatureLogger :: GroupAddress -> Device
temperatureLogger ga = makeDevice "Temperature Logger" () $ temperatureLoggerF ga

temperatureLoggerF :: GroupAddress -> DeviceM () ()
temperatureLoggerF ga = do
    groupRead ga
    eventLoop (groupValue ga parseDPT9) $ \(DPT9 a) -> do
        debug $ "Temperature is " ++ show a ++ "°C"

blindsDeviceKitchen :: Device
blindsDeviceKitchen = makeBlindsDevice "Sonnenschutz Küche" blindsConfigKitchen

blindsConfigKitchen = BlindsConfig
    { upDownGA = GroupAddress 2 2 3
    , stopGA = GroupAddress 2 3 3
    , positionGA = GroupAddress 2 4 3
    , positionStateGA = GroupAddress 2 5 3
    , openGA = GroupAddress 2 1 39
    , closeGA = GroupAddress 2 1 40
    , timeToMove = 19
    , motorStartDelay = 3
    }