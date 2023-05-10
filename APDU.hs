module APDU
    ( APDU (..)
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS

data APDU = APDU
    { tpci :: Word8
    , apci :: Word8
    , payload :: [Word8]
    } deriving (Show)

instance Binary APDU where
    get = do
        tpci <- getWord8
        apci <- getWord8
        payload <- LBS.unpack <$> getRemainingLazyByteString
        return APDU { tpci = tpci
                    , apci = apci
                    , payload = payload
                    }
    put apdu = do
        putWord8 $ tpci apdu
        putWord8 $ apci apdu
        putLazyByteString $ LBS.pack $ payload apdu