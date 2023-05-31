{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hue.Datatypes
    ( Metadata (..)
    , Room (..)
    , Scene (..)
    , Response (..)
    , Service (..)
    , Group (..)
    ) where

import Data.Aeson
import Data.UUID
import qualified Data.ByteString.Lazy as L
import GHC.Generics

data Metadata = Metadata
  { metadataName :: String
  } deriving (Show, Generic)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v -> Metadata
    <$> v .: "name"

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

data Response a = Response
  { responseErrors :: [String]
  , responseData :: [a]
  } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v -> Response
    <$> v .: "errors"
    <*> v .: "data"