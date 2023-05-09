
import Control.Exception (try, SomeException)
import System.IO
import Control.Monad (unless)
import Data.Maybe (fromJust, isJust)
import Data.Word
import Data.Bits
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Concurrent (forkIO)
import KNXAddress
import KNX
import Data.Time.Clock (UTCTime, getCurrentTime, utctDayTime, utctDay)
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Calendar (toGregorian)
import Data.Time
import Control.Concurrent (threadDelay)

knxGatewayHost = "localhost"
knxGatewayPort = "6720"

stdinLoop :: Handle -> KNXConnection -> IO ()
stdinLoop handle knx = do
  line <- hGetLine handle
  putStrLn $ "Received from stdin: " ++ line
  -- Parse "1/2/3 0 0 0 ..." to KNXAdress + List of integers
  let parts = words line
  case parseInput parts of
    Just (groupAddress, byteList) -> do
      -- Do something with the parsed values
      putStrLn $ "Parsed GroupAddress: " ++ show groupAddress
      putStrLn $ "Parsed Byte List: " ++ show byteList
      let telegram = Telegram
            { messageCode = 39
            , additionalInfo = Nothing
            , srcField = Nothing
            , dstField = groupAddress
            , tpci = 0x00
            , apci = 0x80
            , payload = LBS.pack (map fromIntegral byteList)
            } 
      putStrLn $ show telegram
      sendTelegram knx telegram
      return ()
    Nothing -> putStrLn "Failed to parse input. Format should be: main/middle/sub byte1 byte2 byte3 ..."

  stdinLoop handle knx

parseInput :: [String] -> Maybe (GroupAddress, [Int])
parseInput (groupAddressStr:byteStrs) = do
  groupAddress <- parseGroupAddressStr groupAddressStr
  bytes <- traverse readMaybe byteStrs
  return (groupAddress, bytes)
parseInput _ = Nothing

main :: IO ()
main = do
  knx <- connectKnx knxGatewayHost knxGatewayPort
  putStrLn "Connected to KNX gateway."

  knxThread <- forkIO $ runKnxLoop knx
  timeThread <- forkIO $ timeSender knx

  stdinLoop stdin knx

  disconnectKnx knx
  putStrLn "Closed connection."


-- Time sender thread
-- Get the current time and send it to the KNX gateway every second
timeSender :: KNXConnection -> IO ()
timeSender knx = do
  putStrLn "Starting time sender thread"
  timeSenderLoop knx

timeSenderLoop :: KNXConnection -> IO ()
timeSenderLoop knx = do
  putStrLn "Sending time"
  time <- getCurrentTime
  let timeBytes = timeToBytes time
  let telegram = Telegram
        { messageCode = 39
        , additionalInfo = Nothing
        , srcField = Nothing
        , dstField = GroupAddress 0 0 1
        , tpci = 0x00
        , apci = 0x80
        , payload = LBS.pack timeBytes
        } 
  putStrLn $ show telegram
  sendTelegram knx telegram
  let dateBytes = dateToBytes time
  let dateTelegram = Telegram
        { messageCode = 39
        , additionalInfo = Nothing
        , srcField = Nothing
        , dstField = GroupAddress 0 0 2
        , tpci = 0x00
        , apci = 0x80
        , payload = LBS.pack dateBytes
        }
  putStrLn $ show dateTelegram
  sendTelegram knx dateTelegram
  threadDelay 5000000
  timeSenderLoop knx

timeToBytes :: UTCTime -> [Word8]
timeToBytes time = [hour, minute, second]
  where
    dayTime = utctDayTime time
    TimeOfDay h m s = timeToTimeOfDay dayTime
    hour = fromIntegral h :: Word8
    minute = fromIntegral m :: Word8
    second = floor s :: Word8

dateToBytes :: UTCTime -> [Word8]
dateToBytes date = [day, month, year]
  where
    (y, m, d) = toGregorian $ utctDay date
    day = fromIntegral d :: Word8
    month = fromIntegral m :: Word8
    year = fromIntegral (y `mod` 100) :: Word8