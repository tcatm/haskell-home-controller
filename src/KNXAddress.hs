{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module KNXAddress
    ( KNXAddress (..)
    , parseKNXAddress
    , parseKNXAddressStr
    , encodeKNXAddress
    , GroupAddress (..)
    , parseGroupAddress
    , parseGroupAddressStr
    , encodeGroupAddress
    ) where

import Data.Bits
import Data.Word
import Data.List.Split (splitOn)
import Data.String (fromString)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap ()
import Data.Aeson.Encoding (text)
import GHC.Generics

data KNXAddress = KNXAddress
    { mainKA :: Int
    , middleKA :: Int
    , subKA :: Int
    } deriving (Ord, Eq, Generic)

instance ToJSON KNXAddress where
  toJSON (KNXAddress main middle sub) = toJSON $ showTriplet main middle sub '.'

showTriplet :: Int -> Int -> Int -> Char -> String
showTriplet main middle sub sep = printf "%d%c%d%c%d" main sep middle sep sub

instance Show KNXAddress where
  show (KNXAddress main middle sub) = "KNXAddress " <> showTriplet main middle sub '.'

instance Read KNXAddress where
  readsPrec _ str = case parseKNXAddressStr str of
    Just ka -> [(ka, "")]
    Nothing -> []

data GroupAddress = GroupAddress
    { mainGA :: Int
    , middleGA :: Int
    , subGA :: Int
    } deriving (Ord, Eq, Generic)

instance ToJSON GroupAddress where
  toJSON (GroupAddress main middle sub) = toJSON $ showTriplet main middle sub '/'

instance Show GroupAddress where
  show (GroupAddress main middle sub) = "GroupAddress " <> showTriplet main middle sub '/'

instance Read GroupAddress where
  readsPrec _ str = case parseGroupAddressStr str of
    Just ga -> [(ga, "")]
    Nothing -> []

instance ToJSONKey GroupAddress where
  toJSONKey = ToJSONKeyText
    (\(GroupAddress main middle sub) ->
         fromString $ showTriplet main middle sub '/')
    (text . (\(GroupAddress main middle sub) ->
         fromString $ showTriplet main middle sub '/'))

conv :: Int -> Int -> Int -> Int -> Int -> Word16
conv s1 s2 mainG middleG subG = fromIntegral $ (mainG `shiftL` s1) + (middleG `shiftL` s2) + subG

encodeKNXAddress :: KNXAddress -> Word16
encodeKNXAddress (KNXAddress main middle sub) = conv 12 8 main middle sub

encodeGroupAddress :: GroupAddress -> Word16
encodeGroupAddress (GroupAddress main middle sub) = conv 11 8 main middle sub

parseKNXAddress :: Word16 -> KNXAddress
parseKNXAddress w = KNXAddress mainKA middleKA subKA
  where
    mainKA = fromIntegral $ w `shiftR` 12
    middleKA = fromIntegral $ (w `shiftR` 8) .&. 0xF
    subKA = fromIntegral $ w .&. 0xFF

parseGroupAddress :: Word16 -> GroupAddress
parseGroupAddress w = GroupAddress mainGA middleGA subGA
  where
    mainGA = fromIntegral $ w `shiftR` 11
    middleGA = fromIntegral $ (w `shiftR` 8) .&. 0x7
    subGA = fromIntegral $ w .&. 0xFF

parseAddressStr :: String -> String -> Maybe (Int, Int, Int)
parseAddressStr sep str =
  case splitOn sep str of
    [mainStr, middleStr, subStr] -> do
      mainKA <- readMaybe mainStr
      middleKA <- readMaybe middleStr
      subKA <- readMaybe subStr
      return (mainKA, middleKA, subKA)
    _ -> Nothing

parseKNXAddressStr :: String -> Maybe KNXAddress
parseKNXAddressStr = fmap (uncurry3 KNXAddress) . parseAddressStr "."

parseGroupAddressStr :: String -> Maybe GroupAddress
parseGroupAddressStr = fmap (uncurry3 GroupAddress) . parseAddressStr "/"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
