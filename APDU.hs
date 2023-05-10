module APDU
    ( APDU (..)
    ) where

import DPTs

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS

data APDU = APDU
    { tpci :: Word8
    , apci :: Word16
    , payload :: DPT
    } deriving (Show)

instance Binary APDU where
    get = do
        word <- getWord16be
        let tpci = fromIntegral $ word `shiftR` 14
        empty <- isEmpty
        (payload, apci) <- do
            if empty
                then do
                    let payload = DPT1 $ word .&. 0x1 == 1 -- FIXME for DPT3
                    let apci = word .&. 0x03C0
                    return (payload, apci)
                else do
                    bs <- getRemainingLazyByteString >>= return . LBS.unpack
                    let apci = word .&. 0x03FF
                    -- case length of bs
                    let payload = bytestringToDPT bs
                    return (payload, apci)

        return APDU { tpci = tpci
                    , apci = apci
                    , payload = payload
                    }
    put apdu = do
        let byte1 = (tpci apdu `shiftL` 6) .|. (fromIntegral $ apci apdu `shiftR` 8)
        let byte2 = fromIntegral $ apci apdu .&. 0xFF
        putWord8 byte1

        case payload apdu of
            DPT1 v -> putWord8 $ byte2 .|. (if v then 0x01 else 0x00)
            DPT2 v -> putWord8 $ byte2 .|. (if fst v then 0x02 else 0x00) .&. (if snd v then 0x01 else 0x00)
            -- convert v to 4bit signed integer, then or with byte2
            DPT3 v -> do
                let limitedV = max (-8) (min 7 v)
                    word8V = fromIntegral limitedV :: Word8
                    signBit = (word8V `shiftR` 4) .&. 0x08
                    dataBits = word8V .&. 0x07
                putWord8 $ byte2 .|. dataBits .|. signBit
            DPT4 v -> do
                putWord8 byte2
                putWord8 $ fromIntegral $ fromEnum v
            DPT5 v -> do
                putWord8 byte2
                putWord8 v
            DPT6 v -> do
                putWord8 byte2
                putWord8 $ fromIntegral v
            DPT7 v -> do
                putWord8 byte2
                putWord16be v
            DPT8 v -> do
                putWord8 byte2
                putWord16be $ fromIntegral v
            DPT9 v -> do
                putWord8 byte2
                -- FIXME float16 not implemented
            DPT10 (a, b, c, d) -> do
                putWord8 byte2
                putWord8 $ fromIntegral $ (a `shiftL` 5) .|. b
                putWord8 $ fromIntegral c
                putWord8 $ fromIntegral d
            DPT11 (a, b, c) -> do
                putWord8 byte2
                putWord8 $ fromIntegral a
                putWord8 $ fromIntegral b
                putWord8 $ fromIntegral c
            DPT12 v -> do
                putWord8 byte2
                putWord32be v
            DPT13 v -> do
                putWord8 byte2
                putWord32be $ fromIntegral v
            DPT14 v -> do
                putWord8 byte2
                -- FIXME float32 not implemented
            DPT15 v -> do
                putWord8 byte2
                putWord32be v
            DPT16 v -> do
                putWord8 byte2
                putStringUtf8 v

-- case on length of list
-- 1 -> DPT5
-- 2 -> DPT7
-- 4 -> DPT9
-- 3 -> DPT 11
-- more -> DPT16
bytestringToDPT :: [Word8] -> DPT
bytestringToDPT bs =
    case length bs of
        1 -> DPT5 . fromIntegral $ head bs
        2 -> DPT7 . fromIntegral $ (bs !! 0) `shiftL` 8 .|. (bs !! 1)
        3 -> DPT11 . (\[a, b, c] -> (a, b, c)) $ map fromIntegral bs
        4 -> DPT12 . fromIntegral $ (bs !! 0) `shiftL` 24 .|. (bs !! 1) `shiftL` 16 .|. (bs !! 2) `shiftL` 8 .|. (bs !! 3)
        _ -> DPT16 $ map (toEnum . fromIntegral) bs