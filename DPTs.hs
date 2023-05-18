module DPTs
    ( DPT (..)
    , EncodedDPT (..)
    , encodeDPT
    , getDPT1
    , getDPT2
    , getDPT3
    , getDPT5
    , getDPT5_1
    , getDPT6
    , getDPT9
    , getDPT18_1
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString.Lazy as LBS

import Data.Time.Clock
import Data.Time.Calendar

import KNXDatatypes

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
         | DPT15 Word32
         | DPT16 String
         | DPT18_1 (Bool, Int)
            deriving (Eq, Show)

data EncodedDPT = EncodedDPT
    { encodedDPT :: LBS.ByteString
    , encodedDPTIsShort :: Bool
    } deriving (Show)

encodeDPT :: DPT -> EncodedDPT
encodeDPT dpt =
    let (result, bool) = case dpt of
                            DPT1 v -> (putWord8 $ if v then 0x01 else 0x00, True)
                            DPT2 v -> (putWord8 $ (if fst v then 0x02 else 0x00) .|. (if snd v then 0x01 else 0x00), True)
                            DPT3 v ->
                                let limitedV = max (-8) (min 7 v)
                                    word8V = fromIntegral limitedV :: Word8
                                    signBit = (word8V `shiftR` 4) .&. 0x08
                                    dataBits = word8V .&. 0x07
                                in (putWord8 $ signBit .|. dataBits, True)
                            DPT4 v -> (putWord8 $ fromIntegral $ fromEnum v, False)
                            DPT5 v -> (putWord8 v, False)
                            DPT5_1 v -> (putWord8 $ fromIntegral $ round (v * 255), False)
                            DPT6 v -> (putWord8 $ fromIntegral v, False)
                            DPT7 v -> (putWord16be v, False)
                            DPT8 v -> (putWord16be $ fromIntegral v, False)
                            DPT9 v -> (putKNXFloat16 v, False)
                            DPT10 v -> (put v, False)
                            DPT11 v -> let (year, month, day) = toGregorian v
                                       in ( do
                                            putWord8 $ fromIntegral day
                                            putWord8 $ fromIntegral month
                                            putWord8 $ fromIntegral $ year - 1900
                                        , False)
                            DPT12 v -> (putWord32be v, False)
                            DPT13 v -> (putWord32be $ fromIntegral v, False)
                            DPT16 v -> (putStringUtf8 v, False)
                            DPT18_1 (a, b) ->
                                ( do
                                    let byte = fromIntegral b :: Word8
                                        c = if a then 0x80 else 0x00
                                    putWord8 $ c .|. byte
                                , False)
    in EncodedDPT (runPut result) bool

getDPT1 :: Get DPT
getDPT1 = DPT1 . (/= 0) <$> getWord8

getDPT2 :: Get DPT
getDPT2 = (\v -> DPT2 ((v .&. 0x02 /= 0), (v .&. 0x01 /= 0))) <$> getWord8

getDPT3 :: Get DPT
getDPT3 = (\v -> DPT3 $ fromIntegral ((v .&. 0x08 `shiftR` 4) .|. (v .&. 0x07))) <$> getWord8

getDPT5 :: Get DPT
getDPT5 = DPT5 <$> getWord8

getDPT5_1 :: Get DPT
getDPT5_1 = DPT5_1 . (/ 255) . fromIntegral <$> getWord8

getDPT6 :: Get DPT
getDPT6 = DPT6 . fromIntegral <$> getInt8

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

getDPT18_1 :: Get DPT
getDPT18_1 = (\v -> DPT18_1 ((v .&. 0x80 /= 0), fromIntegral $ v .&. 0x7F)) <$> getWord8