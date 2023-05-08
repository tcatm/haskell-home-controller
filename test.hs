import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import Control.Exception (try, SomeException)
import System.IO
import Control.Monad (unless)
import Data.Binary.Get
import Data.Maybe (fromJust, isJust)
import Data.Word
import Data.Int
import Data.Bits
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Concurrent (forkIO)
import GroupAddress

knxGatewayHost :: HostName
knxGatewayHost = "localhost"

knxGatewayPort :: ServiceName
knxGatewayPort = "6720"

eibOpenGroupcon :: Word
eibOpenGroupcon = 0x26

data Telegram = Telegram
    { typeField :: Word16
    , srcField :: GroupAddress
    , dstField :: GroupAddress
    , unkField :: Word16
    , cmdData :: LBS.ByteString
    }

instance Show Telegram where
  show (Telegram t src dst unk cmd) = concat
    [ "Telegram { typeField = ", show t
    , ", srcField = ", show src
    , ", dstField = ", show dst
    , ", unkField = ", show unk
    , ", cmdData = ", showCmdData cmd
    , " }"
    ]
    where
      showCmdData :: LBS.ByteString -> String
      showCmdData lbs = show (LBS.unpack lbs :: [Word8])

parseTelegram :: LBS.ByteString -> Either String Telegram
parseTelegram input =
  case runGetOrFail getLength input of
    Left (_, _, err) -> Left err
    Right (remainingInput, _, lengthVal) ->
      case runGetOrFail (getTelegramData (fromIntegral lengthVal)) remainingInput of
        Left (_, _, err) -> Left err
        Right (_, _, telegram) -> Right telegram
  where
    getLength :: Get Word16
    getLength = getWord16be

    getTelegramData :: Int64 -> Get Telegram
    getTelegramData telegramLength = do
      telegramType <- getWord16be
      src <- fmap parseGroupAddress $ getWord16be
      dst <- fmap parseGroupAddress $ getWord16be
      unk <- getWord16be
      payload <- getLazyByteString (telegramLength - 8) -- Subtract 8 because we already read 4 * 2 bytes
      return Telegram { typeField = telegramType, srcField = src, dstField = dst, unkField = unk, cmdData = payload }


composeTelegram (Telegram t src dst unk cmd) = Put.runPut $ do
    let encodedFields = Put.runPut $ putFields t src dst unk cmd
    let encodedLength = fromIntegral $ LBS.length encodedFields :: Word16
    Put.putWord16be encodedLength
    Put.putLazyByteString encodedFields
    where
        putFields t src dst unk cmd = do
            Put.putWord16be t
            Put.putWord16be $ composeGroupAddress dst
            Put.putWord16be unk
            Put.putLazyByteString cmd



data FieldType = UInt16 | UInt8

type Fields = [(FieldType, Word)]

encodeData :: Fields -> LBS.ByteString
encodeData fields = Put.runPut $ do
    let encodedFields = Put.runPut $ putFields fields
    let encodedLength = fromIntegral $ LBS.length encodedFields :: Word16
    Put.putWord16be encodedLength
    Put.putLazyByteString encodedFields
    where
        putFields [] = return ()
        putFields ((fieldType, value) : rest) = do
            case fieldType of
                UInt16 -> Put.putWord16be (fromIntegral value)
                UInt8  -> Put.putWord8 (fromIntegral value)
            putFields rest

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim (x:xs)
    | x == delim = [] : splitOn delim xs
    | otherwise = let (y:ys) = splitOn delim xs in (x:y) : ys

createTCPConnection :: HostName -> ServiceName -> IO Socket
createTCPConnection host port = do
    addrInfos <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)
    return sock

hexWithSpaces :: BS.ByteString -> String
hexWithSpaces = unwords . map (printf "%02x") . BS.unpack

recvLoop :: Socket -> LBS.ByteString -> IO ()
recvLoop sock buffer = do
  msg <- recv sock 1024
  let lazyMsg = LBS.fromStrict msg
  let newBuffer = LBS.append buffer lazyMsg
  if LBS.null lazyMsg
    then putStrLn "Connection closed by the server"
    else do
      case parseTelegram newBuffer of
        Left err -> do
          recvLoop sock newBuffer
        Right telegram -> do
          putStrLn $ "Received: " ++ show telegram
          recvLoop sock LBS.empty -- reset the buffer

stdinLoop :: Handle -> Socket -> IO ()
stdinLoop handle sock = do
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
            { typeField = 39
            , srcField = GroupAddress 0 0 0
            , dstField = groupAddress
            , unkField = 128
            , cmdData = LBS.pack (map fromIntegral byteList)
            } 
      putStrLn $ show telegram
      let msg = composeTelegram telegram
      _ <- send sock (LBS.toStrict msg)
      return ()
    Nothing -> putStrLn "Failed to parse input. Format should be: main/middle/sub byte1 byte2 byte3 ..."

  stdinLoop handle sock

parseInput :: [String] -> Maybe (GroupAddress, [Int])
parseInput (groupAddressStr:byteStrs) = do
  groupAddress <- parseGroupAddressStr groupAddressStr
  bytes <- traverse readMaybe byteStrs
  return (groupAddress, bytes)
parseInput _ = Nothing

parseGroupAddressStr :: String -> Maybe GroupAddress
parseGroupAddressStr str =
  case splitOn '/' str of
    [mainStr, middleStr, subStr] -> do
      mainGroup <- readMaybe mainStr
      middleGroup <- readMaybe middleStr
      subGroup <- readMaybe subStr
      return GroupAddress { mainGroup = mainGroup, middleGroup = middleGroup, subGroup = subGroup }
    _ -> Nothing


main :: IO ()
main = do
  sock <- createTCPConnection knxGatewayHost knxGatewayPort
  putStrLn "Created TCP Connection."

  -- Send a message to the KNX/IP gateway (replace this with the desired KNXnet/IP message)
  let message = encodeData [(UInt16, eibOpenGroupcon), (UInt16, 0), (UInt8, 0)]
  sent <- send sock (LBS.toStrict message)

  knxThread <- forkIO $ recvLoop sock LBS.empty

  stdinLoop stdin sock

  close sock
  putStrLn "Closed connection."