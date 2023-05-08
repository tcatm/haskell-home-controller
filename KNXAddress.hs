module KNXAddress
    ( EncodedKNXAddress
    , KNXAddress (..)
    , parseKNXAddress
    , composeKNXAddress
    , parseKNXAddressStr
    , GroupAddress (..)
    , parseGroupAddress
    , composeGroupAddress
    , parseGroupAddressStr
    ) where

import Data.Bits
import Data.Word
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Text.Read (readMaybe)

data KNXAddress = KNXAddress
    { mainKA :: Int
    , middleKA :: Int
    , subKA :: Int
    } deriving (Eq)

showTriplet :: Int -> Int -> Int -> Char -> String
showTriplet main middle sub sep = printf "%d%c%d%c%d" main sep middle sep sub

instance Show KNXAddress where
  show (KNXAddress main middle sub) = showTriplet main middle sub '.'

data GroupAddress = GroupAddress
    { mainGA :: Int
    , middleGA :: Int
    , subGA :: Int
    } deriving (Eq)

instance Show GroupAddress where
  show (GroupAddress main middle sub) = showTriplet main middle sub '/'

type EncodedKNXAddress = Int

knxAddressToGroupAddress :: KNXAddress -> GroupAddress
knxAddressToGroupAddress (KNXAddress main middle sub) = GroupAddress main middle sub

conv :: Int -> Int -> Int -> EncodedKNXAddress
conv mainG middleG subG = (mainG `shiftL` 11) + (middleG `shiftL` 8) + subG

parseKNXAddress :: Word16 -> KNXAddress
parseKNXAddress w = KNXAddress mainKA middleKA subKA
  where
    mainKA = fromIntegral $ (w `shiftR` 11) .&. 0x1F
    middleKA = fromIntegral $ (w `shiftR` 8) .&. 0x07
    subKA = fromIntegral $ w .&. 0xFF

composeKNXAddress :: KNXAddress -> Word16
composeKNXAddress (KNXAddress main middle sub) = fromIntegral $ conv main middle sub

parseGroupAddress :: Word16 -> GroupAddress
parseGroupAddress = knxAddressToGroupAddress . parseKNXAddress

composeGroupAddress :: GroupAddress -> Word16
composeGroupAddress (GroupAddress main middle sub) = composeKNXAddress $ KNXAddress main middle sub

parseAddressStr :: String -> String -> Maybe KNXAddress
parseAddressStr sep str =
  case splitOn sep str of
    [mainStr, middleStr, subStr] -> do
      mainKA <- readMaybe mainStr
      middleKA <- readMaybe middleStr
      subKA <- readMaybe subStr
      return KNXAddress { mainKA = mainKA, middleKA = middleKA, subKA = subKA }
    _ -> Nothing

parseKNXAddressStr :: String -> Maybe KNXAddress
parseKNXAddressStr = parseAddressStr "."

parseGroupAddressStr :: String -> Maybe GroupAddress
parseGroupAddressStr str = knxAddressToGroupAddress <$> parseAddressStr "/" str
