{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module KNX 
    ( connectKnx
    , disconnectKnx
    , runKnxLoop
    , KNXConnection (..)
    , KNXCallback (..)
    , KNXM (..)
    , runKNX
    , logSourceKNX
    , emit
    ) where

import APDU
import DPTs
import KNXTelegram
import KNXAddress
import KNXMessages

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import Data.Text (pack)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Binary as B
import Data.Binary.Get
import Data.Binary.Put
import Text.Printf (printf)
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Data.Conduit hiding (connect)
import qualified Data.Conduit.List as CL

logSourceKNX :: LogSource
logSourceKNX = "KNX"

eibOpenGroupcon = 0x26

newtype KNXCallback = KNXCallback (IncomingMessage -> IO ())

newtype KNXM a = KNXM { runKNXM :: ReaderT KNXConnection (LoggingT IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)

runKNX :: KNXConnection -> KNXM a -> (LoggingT IO) a
runKNX knx = flip runReaderT knx . runKNXM

data KNXConnection = KNXConnection
    { host :: HostName
    , port :: ServiceName
    , sock :: Socket
    , callback :: KNXCallback
    }

hexWithSpaces :: LBS.ByteString -> String
hexWithSpaces = unwords . map (printf "%02x") . LBS.unpack

createTCPConnection :: HostName -> ServiceName -> IO Socket
createTCPConnection host port = do
    addrInfos <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)
    return sock

connectKnx :: HostName -> ServiceName -> KNXCallback -> (LoggingT IO) KNXConnection
connectKnx host port cb = do
    sock <- liftIO $ createTCPConnection host port
    -- send eibOpenGroupcon
    let message = composeMessage eibOpenGroupconMessage
    _ <- liftIO $ send sock (LBS.toStrict message)
    logInfoNS logSourceKNX . pack $ "Connected to KNX gateway."
    return KNXConnection { host = host, port = port, sock = sock, callback = cb }

disconnectKnx :: KNXM ()
disconnectKnx = KNXM $ do
    knx <- ask
    liftIO $ close (sock knx)
    logInfoNS logSourceKNX . pack $ "Disconnected from KNX gateway."
    return ()

sourceSocket :: Socket -> ConduitT () BS.ByteString KNXM ()
sourceSocket sock = loop where
    loop = do
        bs <- liftIO $ recv sock 1024
        case BS.length bs of
            -- If we get an empty bytestring, the socket is closed.
            0 -> return ()
            _ -> yield bs >> loop

messageChunks :: ConduitT BS.ByteString BS.ByteString KNXM ()
messageChunks = loop BS.empty
    where
        loop buffer = do
            nextChunk <- await
            case nextChunk of
                Nothing -> when (not $ BS.null buffer) $ leftover buffer  -- Pass along any remaining data
                Just chunk -> do
                    let buffer' = BS.append buffer chunk
                    processBuffer buffer'
        processBuffer buffer
            | BS.length buffer < 2 = loop buffer  -- We need at least two bytes to get the length
            | otherwise = do
                let msgLen = fromIntegral $ runGet getWord16be $ LBS.fromStrict buffer
                    (msg, buffer') = BS.splitAt msgLen $ BS.drop 2 buffer
                if BS.length msg < msgLen
                    then loop buffer  -- We don't have a complete message yet, wait for more data
                    else do
                        yield msg  -- We have a complete message, yield it and continue with the remaining buffer
                        processBuffer buffer'

runKnxLoop :: KNXM ()
runKnxLoop = do
    logInfoNS logSourceKNX . pack $ "Starting KNX loop"
    knx <- KNXM ask
    runConduit $ sourceSocket (sock knx) .| messageChunks .| CL.mapM_ processMessage
    logErrorNS logSourceKNX . pack $ "Socket closed, exiting KNX loop"

processMessage :: BS.ByteString -> KNXM ()
processMessage msg = do
    let telegram = parseMessage $ LBS.fromStrict msg
    case telegram of
        Left err -> logWarnNS logSourceKNX . pack $ "Error parsing message: " <> err
        Right msg -> processTelegram msg

processTelegram :: IncomingMessage -> KNXM ()
processTelegram msg = do
    logDebugNS logSourceKNX . pack $ "Received message: " <> show msg
    KNXCallback cb' <- KNXM $ asks callback
    liftIO $ cb' msg

parseMessage :: LBS.ByteString -> Either String IncomingMessage
parseMessage msg = do
    let messageCode = runGet getWord16be msg
    case messageCode of
        0x27 -> do
            let telegram = B.decode msg :: KNXTelegram
                apci = APDU.apci $ apdu telegram
                groupAddress = dstField telegram
                payload' = payload $ apdu telegram

            case apci of
                ACPIGroupValueRead      -> Right $ IncomingGroupValueRead groupAddress
                ACPIGroupValueResponse  -> Right $ IncomingGroupValueResponse groupAddress payload'
                ACPIGroupValueWrite     -> Right $ IncomingGroupValueWrite groupAddress payload'
                _ -> Left $ "Received unknown APDU: " <> show (apdu telegram)
        _   -> Left $ "Received unknown message code: " <> show messageCode

eibOpenGroupconMessage :: Put
eibOpenGroupconMessage = do
    putWord16be $ fromIntegral eibOpenGroupcon
    putWord16be 0
    putWord8 0

composeMessage :: Put -> LBS.ByteString
composeMessage msg = runPut $ do
    let msgLength = LBS.length msg'
    putWord16be $ fromIntegral msgLength
    putLazyByteString msg'

    where
        msg' = runPut msg

sendTelegram :: KNXTelegram -> KNXM ()
sendTelegram telegram = do
    knx <- KNXM ask
    let msg = composeMessage $ B.put telegram
    logDebugNS logSourceKNX . pack $ "Sending: " <> hexWithSpaces msg
    _ <- liftIO $ send (sock knx) (LBS.toStrict msg)

    -- Echo the message back to the callback
    case apci $ apdu telegram of
        ACPIGroupValueRead      -> processTelegram $ IncomingGroupValueRead (dstField telegram)
        ACPIGroupValueResponse  -> processTelegram $ IncomingGroupValueResponse (dstField telegram) (payload $ apdu telegram)
        ACPIGroupValueWrite     -> processTelegram $ IncomingGroupValueWrite (dstField telegram) (payload $ apdu telegram)
        _    -> return ()

    return ()

composeTelegram :: ACPI -> GroupAddress -> Maybe DPT -> KNXTelegram
composeTelegram apci groupAddress mdpt =
    KNXTelegram  { messageCode = 39
                 , srcField = Nothing
                 , dstField = groupAddress
                 , apdu = APDU   { tpci = 0x00
                                 , apci = apci
                                 , APDU.payload = payload
                                 }
                 }
    where
        payload = case mdpt of
            Just dpt -> encodeDPT dpt
            Nothing -> EncodedDPT (LBS.pack [0]) True

emit :: GroupMessage -> KNXM ()
emit (GroupValueWrite groupAddress dpt) = do
    logDebugNS logSourceKNX . pack $ "Writing " <> show dpt <> " to " <> show groupAddress
    sendTelegram $ composeTelegram ACPIGroupValueWrite groupAddress (Just dpt)

emit (GroupValueRead groupAddress) = do
    logDebugNS logSourceKNX . pack $ "Reading from " <> show groupAddress
    sendTelegram $ composeTelegram ACPIGroupValueRead groupAddress Nothing

emit (GroupValueResponse groupAddress dpt) = do
    logDebugNS logSourceKNX . pack $ "Responding to " <> show groupAddress <> " with " <> show dpt
    sendTelegram $ composeTelegram ACPIGroupValueResponse groupAddress (Just dpt)