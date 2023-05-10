module KNX 
    ( knxLoop
    , connectKnx
    , disconnectKnx
    , sendTelegram
    , runKnxLoop
    , KNXConnection (..)
    ) where

import Telegram

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Text.Printf (printf)

eibOpenGroupcon = 0x26

data KNXConnection = KNXConnection
    { host :: HostName
    , port :: ServiceName
    , sock :: Socket
    } deriving (Show)

hexWithSpaces :: LBS.ByteString -> String
hexWithSpaces = unwords . map (printf "%02x") . LBS.unpack

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
        let msgLength = fromIntegral $ runGet getWord16be newBuffer
        if msgLength > fromIntegral (LBS.length newBuffer)
            then do
                knxLoop knx newBuffer
        else do
            let msg = LBS.drop 2 newBuffer
            let (telegramBytes, rest) = LBS.splitAt msgLength msg
            let messageCode = runGet getWord16be telegramBytes
            if messageCode == 0x27
                then do
                let telegram = decode telegramBytes :: Telegram
                putStrLn $ "Received telegram: " ++ show telegram
            else do
                putStrLn $ "Received unknown message code: " ++ show messageCode
            knxLoop knx rest

eibOpenGroupconMessage :: LBS.ByteString
eibOpenGroupconMessage = runPut $ do
    putWord16be $ fromIntegral eibOpenGroupcon
    putWord16be 0
    putWord8 0

composeMessage :: LBS.ByteString -> LBS.ByteString
composeMessage msg = runPut $ do
    let msgLength = fromIntegral $ LBS.length msg
    putWord16be $ fromIntegral msgLength
    putLazyByteString msg

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
    let msg = composeMessage $ encode telegram
    -- putStrLn $ "Sending: " ++ hexWithSpaces msg
    _ <- send (sock knxConnection) (LBS.toStrict msg)
    return ()