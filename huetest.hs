{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hue where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data LightState = LightState 
  { on  :: Bool
  , bri :: Int
  } deriving (Show, Generic)

data HueLight = HueLight 
  { state :: LightState
  } deriving (Show, Generic)

instance FromJSON LightState
instance FromJSON HueLight

-- Your Hue Bridge's IP
hueBridgeIp :: String
hueBridgeIp = "192.168.180.27"

-- Your API Key (also known as a username)
hueApiKey :: String
hueApiKey = "HYfYqKpZHq1xLEOUfyES2NtW37MkDmsoOyEcJe21"

-- Get the status of a light
getLight :: Int -> IO (Maybe HueLight)
getLight lightNumber = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ "http://" ++ hueBridgeIp ++ "/api/" ++ hueApiKey ++ "/lights/" ++ show lightNumber
  response <- httpLbs request manager
  return $ decode (responseBody response)

-- Set the status of a light
setLight :: Int -> LightState -> IO ()
setLight lightNumber newState = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ "http://" ++ hueBridgeIp ++ "/api/" ++ hueApiKey ++ "/lights/" ++ show lightNumber ++ "/state"
  let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS $ encode newState }
  response <- httpLbs request manager
  return ()
