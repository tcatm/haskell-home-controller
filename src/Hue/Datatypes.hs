{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hue.Datatypes
    ( State (..)
    , HueObject (..)
    , Metadata (..)
    , Room (..)
    , Scene (..)
    , HueResponse (..)
    , Service (..)
    , Group (..)
    , GroupedLight (..)
    , Zone (..)
    , Event (..)
    , EventType (..)
    , EventData (..)
    , GroupedLightUpdate (..)
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map as Map
import Data.UUID
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import GHC.Generics

class HueObject a where
  getId :: a -> UUID

data State = State
  { stateRooms :: Map.Map UUID Room
  , stateScenes :: Map.Map UUID Scene
  , stateGroupedLights :: Map.Map UUID GroupedLight
  , stateZones :: Map.Map UUID Zone
  } deriving (Show)

data Metadata = Metadata
  { metadataName :: String
  } deriving (Show, Generic)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v -> Metadata
    <$> v .: "name"

data On = On
  { onOn :: Bool
  } deriving (Show, Generic)

instance FromJSON On where
  parseJSON = withObject "On" $ \v -> On
    <$> v .: "on"

instance ToJSON On where
  toJSON (On on) = object
    [ "on" .= on
    ]

data Dimming = Dimming
  { dimmingBrightness :: Double
  } deriving (Show, Generic)

instance FromJSON Dimming where
  parseJSON = withObject "Dimming" $ \v -> Dimming
    <$> v .: "brightness"

instance ToJSON Dimming where
  toJSON (Dimming brightness) = object
    [ "brightness" .= brightness
    ]

data GroupedLight = GroupedLight
  { groupedLightId :: UUID
  , groupedLightOn :: On
  , groupedLightDimming :: Maybe Dimming
  } deriving (Show, Generic)

instance HueObject GroupedLight where
  getId = groupedLightId

instance FromJSON GroupedLight where
  parseJSON = withObject "GroupedLight" $ \v -> GroupedLight
    <$> v .: "id"
    <*> v .: "on"
    <*> v .:? "dimming"

data Room = Room
  { roomId :: UUID
  , roomMetadata :: Metadata
  , roomServices :: [Service]
  } deriving (Show, Generic)

instance HueObject Room where
  getId = roomId

instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> Room
    <$> v .: "id"
    <*> v .: "metadata"
    <*> v .: "services"

data Zone = Zone
  { zoneId :: UUID
  , zoneMetadata :: Metadata
  , zoneServices :: [Service]
  } deriving (Show, Generic)

instance HueObject Zone where
  getId = zoneId

instance FromJSON Zone where
  parseJSON = withObject "Zone" $ \v -> Zone
    <$> v .: "id"
    <*> v .: "metadata"
    <*> v .: "services"

data Scene = Scene
  { sceneId :: UUID
  , sceneMetadata :: Metadata
  , sceneGroup :: Group
  } deriving (Show, Generic)

instance HueObject Scene where
  getId = sceneId

instance FromJSON Scene where
  parseJSON = withObject "Scene" $ \v -> Scene
    <$> v .: "id"
    <*> v .: "metadata"
    <*> v .: "group"

data Group = Group
    { groupRid :: UUID
    , groupRtype :: String
    } deriving (Show, Generic, Eq)

instance HueObject Group where
    getId = groupRid

instance FromJSON Group where
    parseJSON = withObject "Group" $ \v -> Group
        <$> v .: "rid"
        <*> v .: "rtype"

data Service = Service 
  { serviceRid :: UUID
  , serviceRtype :: String
  } deriving (Show, Generic)

instance HueObject Service where
  getId = serviceRid

instance FromJSON Service where
  parseJSON = withObject "Service" $ \v -> Service
    <$> v .: "rid"
    <*> v .: "rtype"

data HueResponse a = HueResponse
  { hueResponseErrors :: [String]
  , hueResponseData :: [a]
  } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (HueResponse a) where
  parseJSON = withObject "Response" $ \v -> HueResponse
    <$> v .: "errors"
    <*> v .: "data"

data EventType = EventAdd | EventDelete | EventUpdate
  deriving (Show, Generic)

instance FromJSON EventType where
  parseJSON = withText "EventType" $ \v -> case v of
    "add" -> return EventAdd
    "delete" -> return EventDelete
    "update" -> return EventUpdate
    _ -> fail "Unknown event type"

data GroupedLightUpdate = GroupedLightUpdate
  { groupedLightUpdateId :: UUID
  , groupedLightUpdateOn :: Maybe On
  , groupedLightUpdateDimming :: Maybe Dimming
  } deriving (Show, Generic)

instance FromJSON GroupedLightUpdate where
  parseJSON = withObject "GroupedLightUpdate" $ \v -> GroupedLightUpdate
    <$> v .: "id"
    <*> v .:? "on"
    <*> v .:? "dimming"

data LightUpdate = LightUpdate
  { lightUpdateId :: UUID
  , lightUpdateOn :: Maybe On
  , lightUpdateDimming :: Maybe Dimming
  } deriving (Show, Generic)

instance FromJSON LightUpdate where
  parseJSON = withObject "LightUpdate" $ \v -> LightUpdate
    <$> v .: "id"
    <*> v .:? "on"
    <*> v .:? "dimming"

data EventData = EventGroupedLight GroupedLightUpdate | EventLight LightUpdate
  deriving (Show, Generic)

instance FromJSON EventData where
  parseJSON = withObject "EventData" $ \v -> do
    type' <- v .: "type" :: Parser Text
    case type' of
      "grouped_light" -> EventGroupedLight <$> (parseJSON $ Object v)
      "light" -> EventLight <$> (parseJSON $ Object v)
      _ -> fail "Unknown event data type"

data Event = Event
  { eventCreationtime :: String
  , eventId :: UUID
  , eventType :: EventType
  , eventData :: [EventData]
  } deriving (Show, Generic)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> Event
    <$> v .: "creationtime"
    <*> v .: "id"
    <*> v .: "type"
    <*> v .: "data"