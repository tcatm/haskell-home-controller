module Main where

import KNX
import DeviceRunner
import Console
import Webinterface
import Config
import Webinterface

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad
import Data.Maybe
import System.Environment (getArgs)

import qualified ElphiWohnung as Elphi
import qualified FM2 as FM2

getConfig :: String -> Config
getConfig "FM2" = FM2.config
getConfig "Elphi" = Elphi.config
getConfig _ = error "Invalid configuration name"

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
main = do
  args <- getArgs
  let mConfig = case args of
        [configName] -> Just $ getConfig configName
        _ -> error "Usage: knx-hs <config>"

  unless (isNothing mConfig) $ do
    let Just config = mConfig
  
    runStdoutLoggingT $ filterLogger logFilter $ do
      deviceInput <- liftIO $ newTChanIO
      webQueue <- liftIO $ newTQueueIO
      knxContext <- createKNXContext knxGatewayHost knxGatewayPort (knxCallback deviceInput)
      
      let actions = [ runKnx knxContext
                    , stdinLoop (sendQueue knxContext)
                    , runDevices (devices config) deviceInput (sendQueue knxContext) webQueue
                    , runWebinterface webQueue
                    ]

      waitAllThreads actions