module KNX 
    ( connectKnx
    , disconnectKnx
    , groupWrite
    , runKnxLoop
    , KNXConnection (..)
    , GroupMessage (..)
    , IncomingGroupMessage (..)
    ) where

import Telegram
import APDU
import KNXAddress
import DPTs

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as B
import Data.Binary.Get
import Data.Binary.Put
import Text.Printf (printf)
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

eibOpenGroupcon = 0x26

data KNXConnection = KNXConnection
    { host :: HostName
    , port :: ServiceName
    , sock :: Socket
    } deriving (Show)

data IncomingGroupMessage = IncomingGroupMessage
    { incomingGroupAddress :: GroupAddress
    , payload :: EncodedDPT
    } deriving (Show)

data GroupMessage = GroupMessage
    { groupAddress :: GroupAddress
    , dpt :: DPT
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

runKnxLoop :: KNXConnection -> MVar IncomingGroupMessage -> IO ()
runKnxLoop knx mVar = do
    putStrLn "Starting KNX loop"
    evalStateT (loop knx mVar) LBS.empty
    where
        loop :: KNXConnection -> MVar IncomingGroupMessage -> StateT LBS.ByteString IO ()
        loop knx mVar = do
            input <- liftIO $ LBS.fromStrict <$> recv (sock knx) 1024
            if LBS.null input
                then do
                    liftIO $ putStrLn "Connection closed by the server"
                    return ()
                else do
                    processInput input mVar
                    loop knx mVar

processInput :: LBS.ByteString -> MVar IncomingGroupMessage -> StateT LBS.ByteString IO ()
processInput input mVar = do
    buffer <- get

    let buffer' = LBS.append buffer input
        msgLength = fromIntegral $ runGet getWord16be buffer'
        msg = LBS.drop 2 buffer'

    newBuffer <- liftIO $ do
        if (LBS.length msg) >= msgLength
            then do
                let (telegramBytes, rest) = LBS.splitAt msgLength msg
                case parseMessage telegramBytes of
                    Left err -> putStrLn $ "Error parsing message: " ++ err
                    Right groupMessage -> putMVar mVar groupMessage
                return rest
            else return buffer'
    put newBuffer

parseMessage :: LBS.ByteString -> Either String IncomingGroupMessage
parseMessage msg = do
    let messageCode = runGet getWord16be msg
    case messageCode of
        0x27 -> do
            let telegram = B.decode msg :: Telegram
                apci = APDU.apci $ apdu telegram
            if apci /= 0x80
                then Left $ "Received unknown APDU: " ++ show (apdu telegram)
                else do
                    let groupAddress = dstField telegram
                    Right $ IncomingGroupMessage { incomingGroupAddress = groupAddress
                                                 , KNX.payload = APDU.payload $ apdu telegram
                                                 }
        _   -> Left $ "Received unknown message code: " ++ show messageCode

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
    let msg = composeMessage $ B.encode telegram
    -- putStrLn $ "Sending: " ++ hexWithSpaces msg
    _ <- send (sock knxConnection) (LBS.toStrict msg)
    return ()

groupWrite :: KNXConnection -> GroupMessage -> IO ()
groupWrite knxConnection (GroupMessage groupAddress dpt) = do
    putStrLn $ "Writing " ++ show dpt ++ " to " ++ show groupAddress
    let telegram = Telegram { messageCode = 39
                            , srcField = Nothing
                            , dstField = groupAddress
                            , apdu = APDU { tpci = 0x00
                                          , apci = 0x80
                                          , APDU.payload = encodeDPT dpt
                                          }
                            }
    sendTelegram knxConnection telegram
    return ()