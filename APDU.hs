module APDU
    ( APDU (..)
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS

data APDU = APDU
    { apci :: Word8
    , tpci :: Word8
    , payload :: LBS.ByteString
    } deriving (Show)

instance Binary APDU where
    get = do
        apci <- getWord8
        tpci <- getWord8
        payload <- getRemainingLazyByteString
        return APDU { apci = apci
                    , tpci = tpci
                    , payload = payload
                    }
    put apdu = do
        putWord8 $ apci apdu
        putWord8 $ tpci apdu
        putLazyByteString $ payload apdu