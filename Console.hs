module Console
    ( stdinLoop
    ) where

import KNX
import KNXAddress
import KNXMessages
import DPTs

import System.IO
import Control.Monad

stdinLoop :: KNXConnection -> IO ()
stdinLoop knx = 
  forever $ do
  line <- hGetLine stdin
  putStrLn $ "Received from stdin: " ++ line
  -- Parse "1/2/3 0 0 0 ..." to KNXAdress + List of integers
  let parts = words line
  case parseInput parts of
    Just (groupAddress, dpt) -> do
      -- Do something with the parsed values
      putStrLn $ "Parsed: " ++ show groupAddress ++ " " ++ show dpt
      runKNX knx $ groupWrite $ GroupValueWrite groupAddress dpt
      return ()
    Nothing -> putStrLn "Failed to parse input. Format should be: main/middle/sub byte1 byte2 byte3 ..."

-- Parse a String like: 0/1/2 DPT1 True
parseInput :: [String] -> Maybe (GroupAddress, DPT)
parseInput (groupAddressStr:dptName:value) = do
  groupAddress <- parseGroupAddressStr groupAddressStr
  let dpt = case dptName of
    -- read and parse to bool
              "DPT1" -> DPT1 $ readBool value
              "DPT2" -> DPT2 $ readBoolTuple value
              "DPT3" -> DPT3 $ read $ head value
              "DPT4" -> DPT4 $ read $ head value
              "DPT5" -> DPT5 $ read $ head value
              "DPT6" -> DPT6 $ read $ head value
              "DPT7" -> DPT7 $ read $ head value
              "DPT8" -> DPT8 $ read $ head value
              "DPT9" -> DPT9 $ read $ head value
              "DPT12" -> DPT12 $ read $ head value
              "DPT13" -> DPT13 $ read $ head value
              "DPT14" -> DPT14 $ read $ head value
              "DPT15" -> DPT15 $ read $ head value
              "DPT16" -> DPT16 $ head value
              "DPT18_1" -> DPT18_1 $ (False, read $ head value)

  return (groupAddress, dpt)

  where
    readBool :: [String] -> Bool
    readBool ["True"] = True
    readBool ["False"] = False
    readBool _ = error "Failed to parse bool"

    readBoolTuple :: [String] -> (Bool, Bool)
    readBoolTuple ["True", "True"] = (True, True)
    readBoolTuple ["True", "False"] = (True, False)
    readBoolTuple ["False", "True"] = (False, True)
    readBoolTuple ["False", "False"] = (False, False)
    readBoolTuple _ = error "Failed to parse bool tuple"
parseInput _ = Nothing
