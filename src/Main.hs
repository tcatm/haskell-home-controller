{-# LANGUAGE OverloadedStrings #-}
module Main where

import KNX
import DeviceRunner
import Console
import Webinterface
import Config
import qualified Hue.Hue as Hue

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad
import Data.Maybe
import System.Environment (getArgs)
import Options.Applicative
import qualified Data.Map.Strict as Map
import Control.Concurrent.Async (async, waitAnyCancel)

import qualified ElphiWohnung as Elphi
import qualified FM2 as FM2

-- Type alias for configuration names
type ConfigName = String

-- A map to store available configurations and their loading functions
availableConfigs :: Map.Map ConfigName Config
availableConfigs = Map.fromList
  [ ("FM2", FM2.config)
  , ("Elphi", Elphi.config)
  ]

getConfigByName :: ConfigName -> Maybe Config
getConfigByName name = Map.lookup name availableConfigs

data Options = Options
  { knxHostname   :: String
  , knxPort       :: String
  , hueConfigFile :: FilePath
  , webPort       :: Int
  , configName    :: ConfigName
  } deriving (Show)

options :: Parser Options
options = Options
  <$> strOption
      ( long "knx-host"
     <> metavar "KNXHOSTNAME"
     <> value "localhost"
     <> help "KNX gateway hostname" )
  <*> strOption
      ( long "knx-port"
     <> metavar "KNXPORT"
     <> value "6720"
     <> help "KNX gateway port" )
  <*> strOption
      ( long "hue-config"
     <> metavar "HUEFILE"
     <> value "hueconfig.ini"
     <> help "Path to the Hue configuration file" )
  <*> option auto
      ( long "web-port"
     <> metavar "WEBPORT"
     <> value 3000
     <> help "Webinterface port" )
  <*> strOption
      ( long "config"
     <> metavar "CONFIG"
     <> help ("Configuration to use: " ++ show (Map.keys availableConfigs)) )

knxCallback :: TChan DeviceInput -> KNXCallback
knxCallback chan = KNXCallback $ atomically <$> writeTChan chan . KNXIncomingMessage

logFilter :: LogSource -> LogLevel -> Bool
logFilter source level
    | source == logSourceKNX && level == LevelDebug = False
    | otherwise = True

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "KNX Home Automation System"
     <> header "knx-hs - A Haskell-based KNX control system" )

run :: Options -> IO ()
run opts = runStdoutLoggingT (filterLogger logFilter $ runApp opts)

runApp :: Options -> LoggingT IO ()
runApp opts = do
  logger <- askLoggerIO
  hueContext <- Hue.initHue (hueConfigFile opts)
  deviceInput <- liftIO newTChanIO
  webQueue <- liftIO newTQueueIO
  knxContext <- createKNXContext (knxHostname opts) (knxPort opts) (knxCallback deviceInput)

  mConfig <- liftIO $ case getConfigByName (configName opts) of
    Just cfg -> return (Just cfg)
    Nothing -> do
      putStrLn ("Error: Invalid configuration name '" ++ configName opts ++ "'. Available configurations are: " ++ show (Map.keys availableConfigs))
      return Nothing

  case mConfig of
    Just config -> liftIO $ do
      a1 <- async $ runLoggingT (runKnx knxContext) logger
      a2 <- async $ runLoggingT (stdinLoop (sendQueue knxContext)) logger
      a3 <- async $ runLoggingT (runDevices (devices config) deviceInput (sendQueue knxContext) webQueue (Hue.sendQueue hueContext)) logger
      a4 <- async $ runLoggingT (runWebinterface webQueue (webPort opts)) logger
      a5 <- async $ runLoggingT (Hue.runHue hueContext) logger
      _ <- waitAnyCancel [a1, a2, a3, a4, a5]
      return ()
    Nothing -> return ()

