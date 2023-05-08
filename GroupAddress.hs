module GroupAddress
    ( GroupAddress (..)
    , parseGroupAddress
    , composeGroupAddress
    , EncodedGroupAddress
    ) where

import Data.Bits
import Data.Word
import Text.Printf (printf)


data GroupAddress = GroupAddress
    { mainGroup :: Int
    , middleGroup :: Int
    , subGroup :: Int
    } deriving (Eq)

parseGroupAddress :: Word16 -> GroupAddress
parseGroupAddress w = GroupAddress main middle sub
  where
    main = fromIntegral $ (w `shiftR` 11) .&. 0x1F
    middle = fromIntegral $ (w `shiftR` 8) .&. 0x07
    sub = fromIntegral $ w .&. 0xFF

composeGroupAddress :: GroupAddress -> Word16
composeGroupAddress (GroupAddress main middle sub) = fromIntegral $ conv main middle sub

instance Show GroupAddress where
  show (GroupAddress main middle sub) = printf "%d/%d/%d" main middle sub


type EncodedGroupAddress = Int

conv :: Int -> Int -> Int -> EncodedGroupAddress
conv mainG middleG subG = (mainG `shiftL` 11) + (middleG `shiftL` 8) + subG
