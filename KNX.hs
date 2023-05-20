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

runKnxLoop :: KNXM ()
runKnxLoop = do
    logInfoNS logSourceKNX . pack $ "Starting KNX loop"
    evalStateT (loop) LBS.empty
    where
        loop = do
            knx <- lift $ KNXM ask
            input <- liftIO $ LBS.fromStrict <$> recv (sock knx) 1024  -- liftIO should be used here
            if LBS.null input
                then do
                    logErrorNS logSourceKNX . pack $ "Connection closed by the server"
                    return ()
                else do
                    processInput input
                    loop

processInput :: LBS.ByteString -> StateT LBS.ByteString KNXM ()
processInput input = do
    buffer <- get

    let buffer' = LBS.append buffer input
        msgLength = fromIntegral $ runGet getWord16be buffer'
        msg = LBS.drop 2 buffer'

    newBuffer <- do
        if (LBS.length msg) >= msgLength
            then do
                let (telegramBytes, rest) = LBS.splitAt msgLength msg
                case parseMessage telegramBytes of
                    Left err -> logWarnNS logSourceKNX . pack $ "Error parsing message: " <> err
                    Right msg -> lift $ processTelegram msg
                return rest
            else return buffer'
    put newBuffer

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