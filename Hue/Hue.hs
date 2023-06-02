{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hue.Hue
    ( initHue
    , runHue
    , HueCommand (..)
    , HueContext (..)
    )
  where

import qualified Hue.Datatypes as HD

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
  , hueState :: TVar HueState
  }

data HueState = HueState
  { stateRooms :: [HD.Room]
  , stateScenes :: [HD.Scene]
  } deriving (Show)

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

      rooms <- getRooms config
      scenes <- getScenes config
      state <- newTVarIO $ HueState rooms scenes

      return $ HueContext config sendQueue state

runHue :: HueContext -> LoggingT IO ()
runHue ctx = runReaderT hueLoop ctx

hueLoop :: ReaderT HueContext (LoggingT IO) ()
hueLoop = forever $ do
  ctx <- ask
  command <- liftIO $ atomically $ readTQueue $ sendQueue ctx

  case command of
    HueCommandScene roomName sceneName -> do
      liftIO $ putStrLn $ "Setting scene " <> sceneName <> " in room " <> roomName
      liftIO $ setScene ctx roomName sceneName
    HueCommandRoom roomName on -> do
      liftIO $ putStrLn $ "Setting room " <> roomName <> " to " <> show on
      liftIO $ setRoom ctx roomName on
      
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

getResponse :: (FromJSON a) => HueConfig -> String -> IO [a]
getResponse config endpoint = do
  putStrLn $ "GET " <> endpoint
  response <- makeRequest config "GET" endpoint ""
  let jsonBody = responseBody response
  let result = eitherDecode jsonBody :: (FromJSON a) => Either String (HD.Response a)
  case result of
    Left errMsg -> do
      putStrLn $ "Error decoding response: " <> errMsg
      return []
    Right resp -> return $ HD.responseData resp

getRooms :: HueConfig -> IO [HD.Room]
getRooms config = getResponse config "/clip/v2/resource/room"

getScenes :: HueConfig -> IO [HD.Scene]
getScenes config = getResponse config "/clip/v2/resource/scene"

filterRoomsByName :: String -> [HD.Room] -> [HD.Room]
filterRoomsByName name rooms = Prelude.filter (\r -> HD.metadataName (HD.roomMetadata r) == name) rooms

setScene :: HueContext -> String -> String -> IO ()
setScene ctx roomName sceneName = do
  state <- readTVarIO $ hueState ctx
  let rooms = filterRoomsByName roomName $ stateRooms state
  let scenes = stateScenes state

  forM_ rooms $ \room -> do
    let scenes' = Prelude.filter (\s -> HD.sceneGroup s == (HD.Group (HD.roomId room) "room")) scenes
    let scenes'' = Prelude.filter (\s -> HD.metadataName (HD.sceneMetadata s) == sceneName) scenes'

    forM_ scenes'' $ \scene -> do
      let url = "/clip/v2/resource/scene/" <> (toString $ HD.sceneId scene)
      let body = object [ "recall" .= object [ "action" .= ("active" :: Text) ] ]
      putStrLn $ "PUT " <> url <> " " <> (show body)
      response <- makeRequest (config ctx) "PUT" url (encode body)
      putStrLn $ show response

setRoom :: HueContext -> String -> Bool -> IO ()
setRoom ctx roomName on = do
  state <- readTVarIO $ hueState ctx
  let rooms = filterRoomsByName roomName $ stateRooms state
  let services = Prelude.concatMap HD.roomServices rooms
  let services' = Prelude.filter (\s -> HD.serviceRtype s == "grouped_light") services
  let serviceIds = Prelude.map HD.serviceRid services'

  let body = object [ "on" .= object [ "on" .= on ] ]

  forM_ serviceIds $ \serviceId -> do
    let url = "/clip/v2/resource/grouped_light/" <> (toString serviceId)
    putStrLn $ "PUT " <> url <> " " <> (show body)
    response <- makeRequest (config ctx) "PUT" url (encode body)
    putStrLn $ show response

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
