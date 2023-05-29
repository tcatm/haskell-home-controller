{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hue.Hue
    ( initHue
    )
  where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Connection
import Control.Exception (handle, IOException)

data LightCommand = LightCommand { commandOn :: Maybe Bool, commandBrightness :: Maybe Int, color :: Maybe ColorPoint } deriving Generic

data ColorPoint = ColorPoint { x :: Float, y :: Float } deriving Generic

instance ToJSON LightCommand
instance ToJSON ColorPoint

data HueConfig = HueConfig
  { bridgeHost :: Text
  , appKey :: Text
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

initHue :: Text -> IO (HueConfig)
initHue configFilename = do
  config <- readConfig configFilename
  let host = bridgeHost config
  let appKey = appKey config
  let config' = HueConfig host appKey
  return config'

prepareRequest :: HueConfig -> C.ByteString -> String -> L.ByteString -> IO (Request)
prepareRequest config method path body = do
  let req = defaultRequest
        { host = C.pack $ unpack $ bridgeHost config
        , secure = True
        , port = 443
        , path = C.pack $ path 
        , method = method
        , requestHeaders = [("hue-application-key", C.pack $ appKey config)]
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
