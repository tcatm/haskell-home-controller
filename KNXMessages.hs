module KNXMessages
    ( IncomingMessage (..)
    , IncomingGroupMessage (..)
    , IncomingGroupValueRead (..)
    , IncomingGroupValueResponse (..)
    , IncomingGroupValueWrite (..)
    , GroupMessage (..)
    , GroupValueRead (..)
    , GroupValueResponse (..)
    , GroupValueWrite (..)
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

class GroupMessage a where
    groupAddress :: a -> GroupAddress
    msgDpt :: a -> Maybe DPT

data GroupValueRead = GroupValueRead
    { gvrAddress :: GroupAddress
    } deriving (Show)

data GroupValueResponse = GroupValueResponse
    { gvrpAddress :: GroupAddress
    , gvrpDpt :: DPT
    } deriving (Show)

data GroupValueWrite = GroupValueWrite
    { gvwAddress :: GroupAddress
    , gvwDpt :: DPT
    } deriving (Show)

instance GroupMessage GroupValueRead where
    groupAddress = gvrAddress
    msgDpt _ = Nothing

instance GroupMessage GroupValueResponse where
    groupAddress = gvrpAddress
    msgDpt = Just . gvrpDpt

instance GroupMessage GroupValueWrite where
    groupAddress = gvwAddress
    msgDpt = Just . gvwDpt
