{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hue.Hue
    ( initHue
    , runHue
    , HueCommand (..)
    , HueContext (..)
    )
  where

import Data.Ini
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Connection
import Control.Exception (handle, IOException)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

data HueCommand = HueCommandScene String String
                deriving (Show)

data HueConfig = HueConfig
  { bridgeHost :: Text
  , appKey :: Text
  }

data HueContext = HueContext
  { config :: HueConfig
  , sendQueue :: TQueue HueCommand
  }

newtype LightId = LightId String

data HueEvent = HueEvent { id :: String, eventType :: String, state :: Maybe HueState } deriving (Show, Generic)
data HueState = HueState { on :: Bool, brightness :: Int } deriving (Show, Generic)

instance FromJSON HueEvent
instance FromJSON HueState

tlsSettings = TLSSettingsSimple
  { settingDisableCertificateValidation = True
  , settingDisableSession = False
  , settingUseServerName = True
  }

readConfig :: String -> IO (Either String HueConfig)
readConfig configFilename = runExceptT $ do
  config <- ExceptT $ readIniFile configFilename

  host <- ExceptT $ return $ lookupValue "hue" "host" config
  appKey <- ExceptT $ return $ lookupValue "hue" "user" config

  return $ HueConfig host appKey

initHue :: String -> IO (HueContext)
initHue configFilename = do
  config <- readConfig configFilename
  case config of
    Left err -> error err
    Right config -> do
      sendQueue <- newTQueueIO
      return $ HueContext config sendQueue

runHue :: HueContext -> LoggingT IO ()
runHue ctx = runReaderT hueLoop ctx

hueLoop :: ReaderT HueContext (LoggingT IO) ()
hueLoop = forever $ do
  ctx <- ask
  command <- liftIO $ atomically $ readTQueue $ sendQueue ctx

  case command of
    HueCommandScene roomName sceneName -> do
      liftIO $ putStrLn $ "Setting scene " <> sceneName <> " in room " <> roomName
--      liftIO $ setScene ctx roomName sceneName
      
prepareRequest :: HueConfig -> C.ByteString -> String -> L.ByteString -> IO (Request)
prepareRequest config method path body = do
  let req = defaultRequest
        { host = encodeUtf8 $ bridgeHost config
        , secure = True
        , port = 443
        , path = C.pack $ path 
        , method = method
        , requestHeaders = [("hue-application-key", encodeUtf8 $ appKey config)]

        , requestBody = RequestBodyLBS body
        }

  return req

makeRequest :: HueConfig -> C.ByteString -> String -> L.ByteString -> IO (Response L.ByteString)
makeRequest config method url body = do
  req <- prepareRequest config method url body
  manager <- newManager $ (mkManagerSettings tlsSettings Nothing)
  httpLbs req manager

listenForEvents :: HueConfig -> IO ()
listenForEvents config = do
  req <- prepareRequest config "GET" eventStreamPath ""
  let req' = req { requestHeaders = [("Accept", "text/event-stream")] <> requestHeaders req }
  manager <- newManager $ (mkManagerSettings tlsSettings Nothing) { managerResponseTimeout = responseTimeoutNone }
  handle handleException $ do
    withResponse req' manager $ \res -> do
      let loop = do
            bs <- brRead $ responseBody res
            if C.null bs
              then return ()
              else do
                putStrLn $ C.unpack bs
                loop
      loop
  where
    handleException :: IOException -> IO ()
    handleException e = do
        putStrLn "Error occurred while listening for events. Retrying..."
        listenForEvents config

eventStreamPath :: String
eventStreamPath = "/eventstream/clip/v2"
