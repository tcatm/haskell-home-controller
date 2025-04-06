module Console
    ( stdinLoop
    ) where

import KNXAddress
import KNXMessages
import DPTs

import System.IO
import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

stdinLoop :: TQueue GroupMessage -> LoggingT IO ()
stdinLoop knx = forever $ do
  line <- liftIO $ hGetLine stdin
  liftIO $ putStrLn $ "Received from stdin: " <> line
  -- Parse "1/2/3 0 0 0 ..." to KNXAdress + List of integers
  let parts = words line
  case parseInput parts of
    Just (groupAddress, dpt) -> do
      -- Do something with the parsed values
      liftIO $ putStrLn $ "Parsed: " <> show groupAddress <> " " <> show dpt
      -- Send a GroupValueWrite to the KNX bus
      liftIO $ atomically $ writeTQueue knx $ GroupValueWrite groupAddress dpt
      return ()
    Nothing -> liftIO $ putStrLn "Failed to parse input. Format should be: main/middle/sub byte1 byte2 byte3 ..."

-- Parse a String like: 0/1/2 DPT1 True
parseInput :: [String] -> Maybe (GroupAddress, DPT)
parseInput (groupAddressStr:dptName:values) = do
  groupAddress <- parseGroupAddressStr groupAddressStr
  dpt <- case dptName of
           "DPT1" -> do
             v <- listToMaybe values
             b <- readBool v
             return $ DPT1 b
           "DPT2" -> case values of
                       (v1:v2:[]) -> do
                         bt <- readBoolTuple v1 v2
                         return $ DPT2 bt
                       _ -> Nothing
           "DPT3" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT3 n
           "DPT4" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT4 n
           "DPT5" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT5 n
           "DPT5_1" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT5_1 n
           "DPT6" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT6 n
           "DPT7" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT7 n
           "DPT8" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT8 n
           "DPT9" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT9 n
           "DPT12" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT12 n
           "DPT13" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT13 n
           "DPT14" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT14 n
           "DPT16" -> do
             v <- listToMaybe values
             return $ DPT16 v
           "DPT18_1" -> do
             v <- listToMaybe values
             n <- readMaybe v
             return $ DPT18_1 (False, n)
           _ -> Nothing
  return (groupAddress, dpt)

  where
    readBool :: String -> Maybe Bool
    readBool "True"  = Just True
    readBool "False" = Just False
    readBool _       = Nothing

    readBoolTuple :: String -> String -> Maybe (Bool, Bool)
    readBoolTuple s1 s2 = do
      b1 <- readBool s1
      b2 <- readBool s2
      return (b1, b2)

parseInput _ = Nothing
