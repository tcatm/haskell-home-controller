module KNXMessages
    ( IncomingMessage (..)
    , GroupMessage (..)
    , eibOpenGroupconMessage
    , parseMessage
    ) where

import KNXAddress
import KNXTelegram
import APDU
import DPTs

import Data.Maybe
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS

-- Incoming messages

data IncomingMessage = IncomingGroupValueRead GroupAddress
                     | IncomingGroupValueResponse GroupAddress EncodedDPT
                     | IncomingGroupValueWrite GroupAddress EncodedDPT
                     deriving (Show)

-- Outgoing messages

data GroupMessage   = GroupValueRead GroupAddress
                    | GroupValueResponse GroupAddress DPT
                    | GroupValueWrite GroupAddress DPT
                    deriving (Show)

eibOpenGroupcon = 0x26

eibOpenGroupconMessage :: Put
eibOpenGroupconMessage = do
    putWord16be $ fromIntegral eibOpenGroupcon
    putWord16be 0
    putWord8 0

parseMessage :: LBS.ByteString -> Either String IncomingMessage
parseMessage msg = do
    let messageCode = runGet getWord16be msg
    case messageCode of
        0x27 -> do
            let telegram = decode msg :: KNXTelegram
                apci = APDU.apci $ apdu telegram
                groupAddress = dstField telegram
                payload' = payload $ apdu telegram

            case apci of
                ACPIGroupValueRead      -> Right $ IncomingGroupValueRead groupAddress
                ACPIGroupValueResponse  -> Right $ IncomingGroupValueResponse groupAddress payload'
                ACPIGroupValueWrite     -> Right $ IncomingGroupValueWrite groupAddress payload'
                _ -> Left $ "Received unknown APDU: " <> show (apdu telegram)
        _   -> Left $ "Received unknown message code: " <> show messageCode
