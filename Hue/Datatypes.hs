{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hue.Datatypes
    ( State (..)
    , Metadata (..)
    , Room (..)
    , Scene (..)
    , HueResponse (..)
    , Service (..)
    , Group (..)
    , GroupedLight (..)
    , Zone (..)
    ) where

import Data.Aeson
import Data.UUID
import qualified Data.ByteString.Lazy as L
import GHC.Generics

data State = State
  { stateRooms :: [Room]
  , stateScenes :: [Scene]
  , stateGroupedLights :: [GroupedLight]
  , stateZones :: [Zone]
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

instance FromJSON Scene where
  parseJSON = withObject "Scene" $ \v -> Scene
    <$> v .: "id"
    <*> v .: "metadata"
    <*> v .: "group"

data Group = Group
    { groupRid :: UUID
    , groupRtype :: String
    } deriving (Show, Generic, Eq)

instance FromJSON Group where
    parseJSON = withObject "Group" $ \v -> Group
        <$> v .: "rid"
        <*> v .: "rtype"

data Service = Service 
  { serviceRid :: UUID
  , serviceRtype :: String
  } deriving (Show, Generic)

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