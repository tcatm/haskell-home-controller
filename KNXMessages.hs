module KNXMessages
    ( IncomingMessage (..)
    , IncomingGroupMessage (..)
    , IncomingGroupValueRead (..)
    , IncomingGroupValueResponse (..)
    , IncomingGroupValueWrite (..)
    , GroupMessage (..)
    ) where

import KNXAddress
import DPTs

import Data.Maybe

-- Incoming messages

data IncomingMessage = IncomingRead IncomingGroupValueRead
                     | IncomingResponse IncomingGroupValueResponse
                     | IncomingWrite IncomingGroupValueWrite
                     deriving (Show)

class IncomingGroupMessage a where
    incomingGA :: a -> GroupAddress
    msgPayload :: a -> Maybe EncodedDPT

data IncomingGroupValueRead = IncomingGroupValueRead
    { igrAddress :: GroupAddress
    } deriving (Show)

data IncomingGroupValueResponse = IncomingGroupValueResponse
    { igvrAddress :: GroupAddress
    , igvrPayload :: EncodedDPT
    } deriving (Show)

data IncomingGroupValueWrite = IncomingGroupValueWrite
    { igvwAddress :: GroupAddress
    , igvwPayload :: EncodedDPT
    } deriving (Show)

instance IncomingGroupMessage IncomingGroupValueRead where
    incomingGA = igrAddress
    msgPayload _ = Nothing

instance IncomingGroupMessage IncomingGroupValueResponse where
    incomingGA = igvrAddress
    msgPayload = Just . igvrPayload

instance IncomingGroupMessage IncomingGroupValueWrite where
    incomingGA = igvwAddress
    msgPayload = Just . igvwPayload

-- Outgoing messages

data GroupMessage   = GroupValueRead GroupAddress
                    | GroupValueResponse GroupAddress DPT
                    | GroupValueWrite GroupAddress DPT
                    deriving (Show)