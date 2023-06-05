{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hue.Hue
    ( initHue
    , runHue
    , logSourceHue
    , HueCommand (..)
    , HueContext (..)
    )
  where

import Hue.Datatypes
import Data.Ini
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Data.UUID
import Data.Map
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HashMap
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

logSourceHue :: LogSource
logSourceHue = "Hue"

data HueCommand = HueCommandScene String String
                | HueCommandRoom String Bool
                deriving (Show)

data HueConfig = HueConfig
  { bridgeHost :: Text
  , appKey :: Text
  } deriving (Show)

data HueContext = HueContext
  { config :: HueConfig
  , sendQueue :: TQueue HueCommand
  , hueState :: TVar State
  }

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

initHue :: String -> LoggingT IO HueContext
initHue configFilename = do
  logInfoNS logSourceHue "Initializing Hue"
  config <- liftIO $ readConfig configFilename
  case config of
    Left err -> error err
    Right config -> do
      sendQueue <- liftIO $ newTQueueIO

      hueState <- initializeState config

      state <- liftIO $ newTVarIO $ hueState

      return $ HueContext config sendQueue state

initializeState :: HueConfig -> LoggingT IO State
initializeState config = do
  logInfoNS logSourceHue "Retrieving initial Hue state"

  rooms <- getResponse config "/clip/v2/resource/room"
  logDebugNS logSourceHue . pack $ "Found rooms: " <> show rooms

  scenes <- getResponse config "/clip/v2/resource/scene"
  logDebugNS logSourceHue . pack $ "Found scenes: " <> show scenes

  groupedLights <- getResponse config "/clip/v2/resource/grouped_light"
  logDebugNS logSourceHue . pack $ "Found grouped lights: " <> show groupedLights

  zones <- getResponse config "/clip/v2/resource/zone"
  logDebugNS logSourceHue . pack $ "Found zones: " <> show zones

  return $ State rooms scenes groupedLights zones

runHue :: HueContext -> LoggingT IO ()
runHue ctx = runReaderT hueLoop ctx

hueLoop :: ReaderT HueContext (LoggingT IO) ()
hueLoop = do
  ctx <- ask
  lift $ loop ctx
  where
    loop ctx = do
      command <- liftIO $ atomically $ readTQueue $ sendQueue ctx

      case command of
        HueCommandScene roomName sceneName -> do
          logDebugNS logSourceHue . pack $ "Setting scene " <> sceneName <> " in room " <> roomName
          setScene ctx roomName sceneName
        HueCommandRoom roomName on -> do
          logDebugNS logSourceHue . pack $ "Setting room " <> roomName <> " to " <> show on
          setRoom ctx roomName on
        
      loop ctx
      
prepareRequest :: HueConfig -> C.ByteString -> String -> L.ByteString -> Request
prepareRequest config method path body =
  defaultRequest
        { host = encodeUtf8 $ bridgeHost config
        , secure = True
        , port = 443
        , path = C.pack $ path 
        , method = method
        , requestHeaders = [("hue-application-key", encodeUtf8 $ appKey config)]
        , requestBody = RequestBodyLBS body
        }

makeRequest :: HueConfig -> C.ByteString -> String -> L.ByteString -> LoggingT IO (Response L.ByteString)
makeRequest config method url body = do
  let req = prepareRequest config method url body
  manager <- liftIO $ newManager $ (mkManagerSettings tlsSettings Nothing)
  liftIO $ httpLbs req manager

getResponse :: (FromJSON a) => HueConfig -> String -> LoggingT IO [a]
getResponse config endpoint = do
  logDebugNS logSourceHue . pack $ "GET " <> endpoint
  response <- makeRequest config "GET" endpoint ""
  let jsonBody = responseBody response
  let result = eitherDecode jsonBody :: (FromJSON a) => Either String (HueResponse a)
  case result of
    Left errMsg -> do
      logWarnNS logSourceHue . pack $ "Error decoding response: " <> errMsg
      return []
    Right resp -> return $ hueResponseData resp

filterRoomsByName :: String -> [Room] -> [Room]
filterRoomsByName name rooms = Prelude.filter (\r -> metadataName (roomMetadata r) == name) rooms

setScene :: HueContext -> String -> String -> LoggingT IO ()
setScene ctx roomName sceneName = do
  state <- lift $ readTVarIO $ hueState ctx
  let rooms = filterRoomsByName roomName $ stateRooms state
  let scenes = stateScenes state

  forM_ rooms $ \room -> do
    let scenes' = Prelude.filter (\s -> sceneGroup s == (Group (roomId room) "room")) scenes
    let scenes'' = Prelude.filter (\s -> metadataName (sceneMetadata s) == sceneName) scenes'

    forM_ scenes'' $ \scene -> do
      let url = "/clip/v2/resource/scene/" <> (toString $ sceneId scene)
      let body = object [ "recall" .= object [ "action" .= ("active" :: Text) ] ]
      logDebugNS logSourceHue . pack $ "PUT " <> url <> " " <> (show body)
      response <- makeRequest (config ctx) "PUT" url (encode body)
      logDebugNS logSourceHue . pack $ show response

setRoom :: HueContext -> String -> Bool -> LoggingT IO ()
setRoom ctx roomName on = do
  state <- lift $ readTVarIO $ hueState ctx
  let rooms = filterRoomsByName roomName $ stateRooms state
  let services = Prelude.concatMap roomServices rooms
  let services' = Prelude.filter (\s -> serviceRtype s == "grouped_light") services
  let serviceIds = Prelude.map serviceRid services'

  let body = object [ "on" .= object [ "on" .= on ] ]

  forM_ serviceIds $ \serviceId -> do
    let url = "/clip/v2/resource/grouped_light/" <> (toString serviceId)
    logDebugNS logSourceHue . pack $ "PUT " <> url <> " " <> (show body)
    response <- makeRequest (config ctx) "PUT" url (encode body)
    logDebugNS logSourceHue . pack $ show response

-- listenForEvents :: HueConfig -> LoggingT IO ()
-- listenForEvents config = do
--   req <- prepareRequest config "GET" eventStreamPath ""
--   let req' = req { requestHeaders = [("Accept", "text/event-stream")] <> requestHeaders req }
--   manager <- newManager $ (mkManagerSettings tlsSettings Nothing) { managerResponseTimeout = responseTimeoutNone }
--   handle handleException $ do
--     withResponse req' manager $ \res -> do
--       let loop = do
--             bs <- brRead $ responseBody res
--             if C.null bs
--               then return ()
--               else do
--                 logDebugNS logSourceHue . pack $ C.unpack bs
--                 loop
--       loop
--   where
--     handleException :: IOException -> LoggingT IO ()
--     handleException e = do
--         logWarnNS logSourceHue "Error occurred while listening for events. Retrying..."
--         listenForEvents config

eventStreamPath :: String
eventStreamPath = "/eventstream/clip/v2"
