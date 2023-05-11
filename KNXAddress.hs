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
import Text.Printf (printf)
import Text.Read (readMaybe)

data KNXAddress = KNXAddress
    { mainKA :: Int
    , middleKA :: Int
    , subKA :: Int
    } deriving (Ord, Eq)

showTriplet :: Int -> Int -> Int -> Char -> String
showTriplet main middle sub sep = printf "%d%c%d%c%d" main sep middle sep sub

instance Show KNXAddress where
  show (KNXAddress main middle sub) = showTriplet main middle sub '.'

data GroupAddress = GroupAddress
    { mainGA :: Int
    , middleGA :: Int
    , subGA :: Int
    } deriving (Ord, Eq)

instance Show GroupAddress where
  show (GroupAddress main middle sub) = showTriplet main middle sub '/'

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
