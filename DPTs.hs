module DPTs
    ( DPT (..)
    , EncodedDPT (..)
    , encodeDPT
    , putDPT
    , getDPT1
    , getDPT2
    , getDPT3
    , getDPT5
    , getDPT5_1
    , getDPT6
    , getDPT7
    , getDPT8
    , getDPT9
    , getDPT10
    , getDPT11
    , getDPT12
    , getDPT13
    , getDPT14
    , getDPT16
    , getDPT18_1
    , getDPT20_102
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Time.Clock
import Data.Time.Calendar

import KNXDatatypes

data EncodedDPT = EncodedDPT
    { encodedDPT :: LBS.ByteString
    , encodedDPTIsShort :: Bool
    } deriving (Show)

data DPT = DPT1 Bool -- short
         | DPT2 (Bool, Bool) --short
         | DPT3 Int -- 4bit, short
         | DPT4 Char
         | DPT5 Word8
         | DPT5_1 Double
         | DPT6 Int8
         | DPT7 Word16
         | DPT8 Int16
         | DPT9 Double
         | DPT10 KNXTimeOfDay
         | DPT11 Day
         | DPT12 Word32
         | DPT13 Int32
         | DPT14 Double
         | DPT16 String
         | DPT18_1 (Bool, Int)
         | DPT20_102 KNXHVACMode
         deriving (Eq, Show, Read)

encodeDPT :: DPT -> EncodedDPT
encodeDPT dpt = EncodedDPT (runPut $ putDPT dpt) (isShort dpt)
    where
        isShort (DPT1 _) = True
        isShort (DPT2 _) = True
        isShort (DPT3 _) = True
        isShort _ = False

putDPT :: DPT -> Put
putDPT (DPT1 v) = putWord8 $ if v then 0x01 else 0x00
putDPT (DPT2 v) = putWord8 $ (if fst v then 0x02 else 0x00) .|. (if snd v then 0x01 else 0x00)
putDPT (DPT3 v) =
    let limitedV = max (-8) (min 7 v)
        word8V = fromIntegral limitedV :: Word8
        signBit = (word8V `shiftR` 4) .&. 0x08
        dataBits = word8V .&. 0x07
    in putWord8 $ signBit .|. dataBits
putDPT (DPT4 v) = putWord8 $ fromIntegral $ fromEnum v
putDPT (DPT5 v) = putWord8 v
putDPT (DPT5_1 v) = putWord8 $ fromIntegral $ round (v * 255)
putDPT (DPT6 v) = putWord8 $ fromIntegral v
putDPT (DPT7 v) = putWord16be v
putDPT (DPT8 v) = putWord16be $ fromIntegral v
putDPT (DPT9 v) = putKNXFloat16 v
putDPT (DPT10 v) = put v
putDPT (DPT11 v) = let (year, month, day) = toGregorian v
            in do
                putWord8 $ fromIntegral day
                putWord8 $ fromIntegral month
                putWord8 $ fromIntegral $ year - 1900
putDPT (DPT12 v) = putWord32be v
putDPT (DPT13 v) = putWord32be $ fromIntegral v
putDPT (DPT14 v) = putDoublebe v
putDPT (DPT16 v) = putLazyByteString $ C.pack v
putDPT (DPT18_1 (a, b)) =
    do
        let byte = fromIntegral b :: Word8
            c = if a then 0x80 else 0x00
        putWord8 $ c .|. byte
putDPT (DPT20_102 v) = put v

getDPT1 :: Get DPT
getDPT1 = DPT1 . (/= 0) <$> getWord8

getDPT2 :: Get DPT
getDPT2 = (\v -> DPT2 ((v .&. 0x02 /= 0), (v .&. 0x01 /= 0))) <$> getWord8

getDPT3 :: Get DPT
getDPT3 = (\v -> DPT3 $ fromIntegral ((v .&. 0x08 `shiftR` 4) .|. (v .&. 0x07))) <$> getWord8

getDPT4 :: Get DPT
getDPT4 = DPT4 . toEnum . fromIntegral <$> getWord8

getDPT5 :: Get DPT
getDPT5 = DPT5 <$> getWord8

getDPT5_1 :: Get DPT
getDPT5_1 = DPT5_1 . (/ 255) . fromIntegral <$> getWord8

getDPT6 :: Get DPT
getDPT6 = DPT6 . fromIntegral <$> getInt8

getDPT7 :: Get DPT
getDPT7 = DPT7 <$> getWord16be

getDPT8 :: Get DPT
getDPT8 = DPT8 . fromIntegral <$> getWord16be

getDPT9 :: Get DPT
getDPT9 = DPT9 <$> getKNXFloat16

getDPT10 :: Get DPT
getDPT10 = DPT10 <$> get

getDPT11 :: Get DPT
getDPT11 = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    let year = fromIntegral c
    let interpretedYear = if year >= 90 then 1900 + year else 2000 + year
    return $ DPT11 $ fromGregorian (fromIntegral interpretedYear) (fromIntegral b) (fromIntegral a)

getDPT12 :: Get DPT
getDPT12 = DPT12 <$> getWord32be

getDPT13 :: Get DPT
getDPT13 = DPT13 . fromIntegral <$> getWord32be

getDPT14 :: Get DPT
getDPT14 = DPT14 <$> getDoublebe

getDPT16 :: Get DPT
getDPT16 = DPT16 <$> C.unpack <$> getRemainingLazyByteString

getDPT18_1 :: Get DPT
getDPT18_1 = (\v -> DPT18_1 ((v .&. 0x80 /= 0), fromIntegral $ v .&. 0x7F)) <$> getWord8

getDPT20_102 :: Get DPT
getDPT20_102 = DPT20_102 <$> get