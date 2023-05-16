module DPTs
    ( DPT (..)
    , EncodedDPT (..)
    , encodeDPT
    , parseDPT1
    , parseDPT2
    , parseDPT3
    , parseDPT5
    , parseDPT6
    , parseDPT18_1
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString.Lazy as LBS

data DPT = DPT1 Bool -- short
         | DPT2 (Bool, Bool) --short
         | DPT3 Int -- 4bit, short
         | DPT4 Char
         | DPT5 Word8
         | DPT6 Int8
         | DPT7 Word16
         | DPT8 Int16
         | DPT9 Double
         | DPT10 (Word, Word, Word, Word) -- 1st Word8 is DayOfWeek (1..7, 0 = no day)
         | DPT11 (Word, Word, Word)
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
                            DPT6 v -> (putWord8 $ fromIntegral v, False)
                            DPT7 v -> (putWord16be v, False)
                            DPT8 v -> (putWord16be $ fromIntegral v, False)
                            DPT9 v -> let (mantissa, exponent) = decodeFloat v
                                          sign = if mantissa < 0 then 1 else 0
                                          mant = fromIntegral $ mantissa .&. 0x7FF
                                          exp = fromIntegral $ exponent + 15
                                      in (putWord16be $ (sign `shiftL` 15) .|. (exp `shiftL` 11) .|. mant, False)
                            DPT10 (a, b, c, d) ->
                                ( do
                                    putWord8 $ fromIntegral $ (a `shiftL` 5) .|. b
                                    putWord8 $ fromIntegral c
                                    putWord8 $ fromIntegral d
                                , False)
                            DPT11 (a, b, c) ->
                                ( do
                                    putWord8 $ fromIntegral a
                                    putWord8 $ fromIntegral b
                                    putWord8 $ fromIntegral c
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

parseDPT1 :: Get DPT
parseDPT1 = DPT1 . (/= 0) <$> getWord8

parseDPT2 :: Get DPT
parseDPT2 = (\v -> DPT2 ((v .&. 0x02 /= 0), (v .&. 0x01 /= (0 :: Word8)))) <$> getWord8

parseDPT3 :: Get DPT
parseDPT3 = (\v -> DPT3 $ fromIntegral ((v .&. 0x08 `shiftR` 4) .|. (v .&. 0x07))) <$> getWord8

parseDPT5 :: Get DPT
parseDPT5 = DPT5 <$> getWord8

parseDPT6 :: Get DPT
parseDPT6 = DPT6 . fromIntegral <$> getInt8

parseDPT9 :: Get DPT
parseDPT9 = do
    mantissaExponent <- getWord16be
    let mantissa = fromIntegral $ mantissaExponent .&. 0x07FF
        exponent = fromIntegral $ (mantissaExponent .&. 0x7800) `shiftR` 11
        sign = if (mantissaExponent .&. 0x8000) /= 0 then -1 else 1
    return $ DPT9 $ encodeFloat (sign * mantissa) (exponent - 15)

parseDPT18_1 :: Get DPT
parseDPT18_1 = (\v -> DPT18_1 ((v .&. 0x80 /= 0), fromIntegral $ v .&. 0x7F)) <$> getWord8