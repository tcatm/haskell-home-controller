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
import Control.Applicative
import Data.Ini
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Data.UUID
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList, fromMaybe, isJust, isNothing)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Connection
import Control.Exception (catch, IOException)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Text.Show.Pretty (ppShow)

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

  rooms <- responseToMap <$> getResponse config "/clip/v2/resource/room"
  logDebugNS logSourceHue . pack $ "Found rooms: " <> show rooms

  scenes <- responseToMap <$> getResponse config "/clip/v2/resource/scene"
  logDebugNS logSourceHue . pack $ "Found scenes: " <> show scenes

  groupedLights <- responseToMap <$> getResponse config "/clip/v2/resource/grouped_light"
  logDebugNS logSourceHue . pack $ "Found grouped lights: " <> show groupedLights

  zones <- responseToMap <$> getResponse config "/clip/v2/resource/zone"
  logDebugNS logSourceHue . pack $ "Found zones: " <> show zones

  return $ State rooms scenes groupedLights zones

responseToMap :: HueObject a => [a] -> Map.Map UUID a
responseToMap = Map.fromList . Prelude.map (\x -> (getId x, x))

runHue :: HueContext -> LoggingT IO ()
runHue ctx = runReaderT hueLoop ctx

hueLoop :: ReaderT HueContext (LoggingT IO) ()
hueLoop = do
  ctx <- ask

  logger <- askLoggerIO

  thread <- liftIO $ forkIO $ do
    runLoggingT (listenForEvents ctx $ Just "0:0") logger

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

filterRoomsByName :: String -> Map.Map UUID Room -> [Room]
filterRoomsByName name = Map.elems . Map.filter (\r -> metadataName (roomMetadata r) == name)

setScene :: HueContext -> String -> String -> LoggingT IO ()
setScene ctx roomName sceneName = do
  state <- lift $ readTVarIO $ hueState ctx
  let rooms = filterRoomsByName roomName $ stateRooms state
  let scenes = stateScenes state

  forM_ rooms $ \room -> do
    let scenes' = Map.filter (\s -> sceneGroup s == (Group (roomId room) "room")) scenes
    let scenes'' = Map.filter (\s -> metadataName (sceneMetadata s) == sceneName) scenes'

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

listenForEvents :: HueContext -> Maybe Text -> LoggingT IO ()
listenForEvents ctx lastEventId = do
  let req = prepareRequest (config ctx) "GET" eventStreamPath ""
  let headers = [ ("Accept", "text/event-stream") ]
             <> maybeToList ((\id -> ("Last-Event-ID", encodeUtf8 id)) <$> lastEventId)
             <> requestHeaders req
  let req' = req { requestHeaders = headers }
  manager <- liftIO $ newManager $ (mkManagerSettings tlsSettings Nothing) { managerResponseTimeout = responseTimeoutNone }
  logger <- askLoggerIO
  lastEventIdVar <- liftIO $ newTVarIO lastEventId
  liftIO $ catch (makeRequest logger lastEventIdVar req' manager) $ \e ->
    runLoggingT ( do 
      handleException lastEventIdVar e
      listenForEvents ctx lastEventId
    ) logger

  where
    makeRequest logger lastEventIdVar req manager = do
      withResponse req manager $ \res -> do
        let loop = do
              bs <- liftIO $ brRead $ responseBody res
              if C.null bs
                then return ()
                else do
                  let messages = splitOnMessages bs
                  mapM_ (handleMessage ctx lastEventIdVar) messages
                  loop

        runLoggingT loop logger

    handleException :: TVar (Maybe Text) -> IOException -> LoggingT IO ()
    handleException lastEventIDVar e = do
        lastEventId <- liftIO $ readTVarIO lastEventIDVar
        logWarnNS logSourceHue "Error occurred while listening for events. Retrying..."

    splitOnMessages :: C.ByteString -> [C.ByteString]
    splitOnMessages bs = splitOnMessages' bs []
      where
        splitOnMessages' bs acc =
          let (msg, rest) = C.breakSubstring "\n\n" bs
          in if C.null rest
               then acc
               else splitOnMessages' (C.drop 2 rest) (acc <> [msg])

    handleMessage :: HueContext -> TVar (Maybe Text) -> C.ByteString -> LoggingT IO ()
    handleMessage ctx lastEventIdVar msg = do
      let lines = C.split '\n' msg

      let lines' = Prelude.map (C.breakSubstring ": ") lines
      let events = Prelude.map (\(k, v) -> (k, C.drop 2 v)) lines'
      let map = Map.fromList events
      let eventId = Map.lookup "id" map

      case eventId of
        Just id -> liftIO $ atomically $ writeTVar lastEventIdVar (Just $ decodeUtf8 id)
        Nothing -> return ()

      let payload = Map.lookup "data" map

      case payload of
        Nothing -> return ()
        Just d -> do
          let json = eitherDecode $ L.fromStrict d :: Either String [Event]
          case json of
            Left err -> do
              logWarnNS logSourceHue $ pack $ "Error decoding event: " <> err <> " " <> show d
              return ()
            Right e -> do
              mapM_ (handleEvent ctx) e

handleEvent :: HueContext -> Event -> LoggingT IO ()
handleEvent ctx event = do
  let data' = eventData event
  liftIO $ putStrLn $ "Event type: " <> (show $ eventType event)
  
  case (eventType event) of
    EventUpdate -> mapM_ (eventUpdate ctx) data'
    _ -> return ()

eventUpdate :: HueContext -> EventData -> LoggingT IO ()
eventUpdate ctx (EventGroupedLight event) = do
  let id = groupedLightUpdateId event

  state <- lift $ readTVarIO $ hueState ctx

  let groupedLight = Map.lookup id $ stateGroupedLights state

  unless (isNothing groupedLight) $ do
    let groupedLight' = fromJust groupedLight
    let groupedLight'' = groupedLight' { groupedLightOn = fromMaybe (groupedLightOn groupedLight') $ groupedLightUpdateOn event
                                       , groupedLightDimming = groupedLightUpdateDimming event <|> groupedLightDimming groupedLight'
                                       }

    -- create device input from event

    let state' = state { stateGroupedLights = Map.insert id groupedLight'' $ stateGroupedLights state }
    lift $ atomically $ writeTVar (hueState ctx) state'

    liftIO $ putStrLn $ "Updated grouped light: " <> (show groupedLight'')

eventUpdate ctx event = do
  logWarnNS logSourceHue $ "Unhandled event: " <> (pack $ show event)

eventStreamPath :: String
eventStreamPath = "/eventstream/clip/v2"
