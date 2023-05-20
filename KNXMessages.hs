module KNXMessages
    ( IncomingMessage (..)
    , GroupMessage (..)
    ) where

import KNXAddress
import DPTs

import Data.Maybe

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