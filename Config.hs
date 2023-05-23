{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}

module Config where

import Device

data Config = Config
    { devices :: [Device]
    }