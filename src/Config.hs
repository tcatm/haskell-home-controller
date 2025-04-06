{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}

module Config where

import DeviceTypes

data Config = Config
    { devices :: [Device]
    }