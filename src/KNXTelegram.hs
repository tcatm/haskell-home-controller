module KNXTelegram
    ( KNXTelegram (..)
    , composeTelegram
    ) where

import APDU
import KNXAddress
import DPTs

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS

data KNXTelegram = KNXTelegram
    { messageCode :: Word8
    , srcField :: Maybe KNXAddress
    , dstField :: GroupAddress
    , apdu :: APDU
    } deriving (Show)

-- Binary encoding is asymmetric.
-- The srcField is not encoded in the telegram. KNXD will fill it in for us.
instance Binary KNXTelegram where
    get = do
        _ <- getWord8
        messageCode <- getWord8
        src <- fmap parseKNXAddress $ getWord16be
        dst <- fmap parseGroupAddress $ getWord16be
        apdu <- decode <$> getRemainingLazyByteString
        return KNXTelegram  { messageCode = messageCode
                            , srcField = Just src
                            , dstField = dst
                            , apdu = apdu
                            }
    put telegram = do
        putWord8 0x00
        putWord8 $ messageCode telegram
        putWord16be $ encodeGroupAddress $ dstField telegram
        putLazyByteString $ encode $ apdu telegram

composeTelegram :: ACPI -> GroupAddress -> Maybe DPT -> KNXTelegram
composeTelegram apci groupAddress mdpt =
    KNXTelegram  { messageCode = 39
                 , srcField = Nothing
                 , dstField = groupAddress
                 , apdu = APDU   { tpci = 0x00
                                 , apci = apci
                                 , APDU.payload = payload
                                 }
                 }
    where
        payload = case mdpt of
            Just dpt -> encodeDPT dpt
            Nothing -> EncodedDPT (LBS.pack [0]) True