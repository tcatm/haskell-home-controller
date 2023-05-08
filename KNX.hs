module KNX 
    ( knxLoop
    , connectKnx
    , disconnectKnx
    , sendTelegram
    , runKnxLoop
    , Telegram (..)
    , KNXConnection (..)
    ) where

import KNXAddress

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe (fromJust, isJust)
import Data.ByteString.Char8 (pack, unpack)
import Text.Printf (printf)
import Text.Read (readMaybe)

eibOpenGroupcon = 0x26

data KNXConnection = KNXConnection
    { host :: HostName
    , port :: ServiceName
    , sock :: Socket
    } deriving (Show)

createTCPConnection :: HostName -> ServiceName -> IO Socket
createTCPConnection host port = do
    addrInfos <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)
    return sock

runKnxLoop :: KNXConnection -> IO ()
runKnxLoop knx = do
    putStrLn "Starting KNX loop"
    knxLoop knx LBS.empty

knxLoop :: KNXConnection -> LBS.ByteString -> IO ()
knxLoop knx buffer = do
    msg <- recv (sock knx) 1024
    let lazyMsg = LBS.fromStrict msg
    let newBuffer = LBS.append buffer lazyMsg
    if LBS.null lazyMsg
        then putStrLn "Connection closed by the server"
    else do
        case parseTelegram newBuffer of
            Left err -> do
                knxLoop knx newBuffer
            Right telegram -> do
                putStrLn $ "Received: " ++ show telegram
                knxLoop knx LBS.empty -- reset the buffer

eibOpenGroupconMessage :: LBS.ByteString
eibOpenGroupconMessage = Put.runPut $ do
    Put.putWord16be $ fromIntegral eibOpenGroupcon
    Put.putWord16be 0
    Put.putWord8 0

composeMessage :: LBS.ByteString -> LBS.ByteString
composeMessage msg = Put.runPut $ do
    let msgLength = fromIntegral $ LBS.length msg
    Put.putWord16be $ fromIntegral msgLength
    Put.putLazyByteString msg

connectKnx :: HostName -> ServiceName -> IO KNXConnection
connectKnx host port = do
    sock <- createTCPConnection host port
    -- send eibOpenGroupcon
    let message = composeMessage eibOpenGroupconMessage
    _ <- send sock (LBS.toStrict message)
    return KNXConnection { host = host, port = port, sock = sock }

disconnectKnx :: KNXConnection -> IO ()
disconnectKnx knxConnection = do
    close (sock knxConnection)
    return ()

sendTelegram :: KNXConnection -> Telegram -> IO ()
sendTelegram knxConnection telegram = do
  let msg = composeTelegram telegram
  _ <- send (sock knxConnection) (LBS.toStrict msg)
  return ()

data Telegram = Telegram
    { typeField :: Word16
    , srcField :: Maybe KNXAddress
    , dstField :: GroupAddress
    , cmdType :: Word16
    , cmdData :: LBS.ByteString
    }

instance Show Telegram where
  show (Telegram t src dst cmdType cmd) = concat
    [ "Telegram { typeField = ", show t
    , ", srcField = ", show src
    , ", dstField = ", show dst
    , ", cmdType = ", show cmdType
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
      src <- fmap parseKNXAddress $ getWord16be
      dst <- fmap parseGroupAddress $ getWord16be
      cmd <- getWord16be
      payload <- getLazyByteString (telegramLength - 8) -- Subtract 8 because we already read 4 * 2 bytes
      return Telegram { typeField = telegramType, srcField = Just src, dstField = dst, cmdType = cmd, cmdData = payload }

composeTelegram :: Telegram -> LBS.ByteString
composeTelegram (Telegram t src dst cmdType cmd) = Put.runPut $ do
    let encodedFields = Put.runPut $ putFields t src dst cmdType cmd
    let encodedLength = fromIntegral $ LBS.length encodedFields :: Word16
    Put.putWord16be encodedLength
    Put.putLazyByteString encodedFields
    where
        putFields t src dst cmdType cmd = do
            Put.putWord16be t
            Put.putWord16be $ composeGroupAddress dst
            Put.putWord16be cmdType
            Put.putLazyByteString cmd
