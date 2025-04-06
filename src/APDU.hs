module APDU
    ( APDU (..)
    , ACPI (..)
    ) where

import DPTs

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS

data ACPI   = ACPIValue Word16
            | ACPIGroupValueRead
            | ACPIGroupValueResponse
            | ACPIGroupValueWrite
            deriving (Show)

fromACPI :: ACPI -> Word16
fromACPI ACPIGroupValueRead = 0x00
fromACPI ACPIGroupValueResponse = 0x40
fromACPI ACPIGroupValueWrite = 0x80
fromACPI (ACPIValue value) = value

toACPI :: Word16 -> ACPI
toACPI 0x00 = ACPIGroupValueRead
toACPI 0x40 = ACPIGroupValueResponse
toACPI 0x80 = ACPIGroupValueWrite
toACPI value = ACPIValue value

data APDU = APDU
    { tpci :: Word8
    , apci :: ACPI
    , payload :: EncodedDPT
    } deriving (Show)

instance Binary APDU where
    get = do
        word <- getWord16be
        let tpci = fromIntegral $ word `shiftR` 14
        empty <- isEmpty
        (payload, apci, short) <- do
            if empty
                then do
                    let apci = word .&. 0x03C0
                        payload = LBS.singleton $ fromIntegral $ word .&. 0x003F
                    return (payload, apci, True)
                else do
                    let apci = word .&. 0x03FF
                    bs <- getRemainingLazyByteString
                    return (bs, apci, False)

        return APDU { tpci = tpci
                    , apci = toACPI apci
                    , payload = EncodedDPT payload short
                    }
    put apdu = do
        let EncodedDPT payload short = APDU.payload apdu
            apci' = fromACPI $ apci apdu
            byte1 = (tpci apdu `shiftL` 6) .|. (fromIntegral $ apci' `shiftR` 8)
            byte2 = fromIntegral $ apci' .&. 0xFF

        putWord8 byte1

        if short
            then do
                let byte = byte2 .|. (fromIntegral $ (LBS.head $ payload) .&. 0x3F)
                putWord8 byte
            else do
                putWord8 byte2
                putLazyByteString $ payload