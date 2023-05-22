{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module KNX 
    ( createKNXContext
    , runKnx
    , KNXCallback (..)
    , KNXContext (..)
    , logSourceKNX
    , sendMessage
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
import Control.Concurrent.STM
import Control.Concurrent.Async (async, wait)
import Data.Conduit hiding (connect)
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.List as CL
import Data.Text (Text)

logSourceKNX :: LogSource
logSourceKNX = "KNX"

newtype KNXCallback = KNXCallback (IncomingMessage -> IO ())

type KNXCtxM = ReaderT KNXContext (LoggingT IO)
type KNXStateM = StateT KNXState KNXCtxM

data KNXState = KNXState
    { sock :: Socket
    }

data KNXContext = KNXContext
    { host :: HostName
    , port :: ServiceName
    , callback :: KNXCallback
    , sendQueue :: TQueue GroupMessage
    }

hexWithSpaces :: LBS.ByteString -> Text
hexWithSpaces = pack . concatMap (printf "%02x ") . LBS.unpack

createTCPConnection :: HostName -> ServiceName -> IO Socket
createTCPConnection host port = do
    addrInfos <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)
    return sock

createKNXContext :: HostName -> ServiceName -> KNXCallback -> LoggingT IO KNXContext
createKNXContext host port cb = do
    sendQueue <- liftIO $ newTQueueIO
    return KNXContext
                { host = host
                , port = port
                , callback = cb
                , sendQueue = sendQueue
                }

connectKnx :: KNXCtxM (Socket)
connectKnx = do
    host <- asks host
    port <- asks port

    sock <- liftIO $ createTCPConnection host port

    -- send eibOpenGroupcon
    let message = composeMessage eibOpenGroupconMessage
    liftIO $ send sock (LBS.toStrict message)

    -- FIXME - check response
    logInfoNS logSourceKNX . pack $ "Connected to KNX gateway."

    return sock

disconnectKnx :: KNXStateM ()
disconnectKnx =  do
    sock <- gets sock
    liftIO $ close sock
    logInfoNS logSourceKNX . pack $ "Disconnected from KNX gateway."
    return ()

messageChunks :: (Monad m) => ConduitT BS.ByteString BS.ByteString m ()
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

runKnx :: KNXContext -> LoggingT IO ()
runKnx ctx = runReaderT runKnxLoop ctx

runKnxLoop :: KNXCtxM ()
runKnxLoop = do
    logInfoNS logSourceKNX . pack $ "Starting KNX loop"

    sock <- connectKnx

    evalStateT knxLoop $ KNXState sock

runKNXStateM :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> KNXState -> KNXContext -> KNXStateM () -> IO ()
runKNXStateM logger state ctx m = do
    runLoggingT (runReaderT (evalStateT m state) ctx) logger

knxLoop :: KNXStateM ()
knxLoop = do
    cb <- lift $ asks callback
    queue <- lift $ asks sendQueue

    let inputConduit sock = recvConduit sock .| CL.mapM_ handleTelegram
    let queueConduit = sourceTQueue queue .| CL.mapM_ handleQueue
    
    logInfoNS logSourceKNX . pack $ "Starting input conduit"

    ctx <- lift $ ask
    state <- get
    logger <- askLoggerIO
    
    a1 <- liftIO $ async $ do
        runKNXStateM logger state ctx $ do
            sock <- gets sock
            runConduit (inputConduit sock)

    logInfoNS logSourceKNX . pack $ "Starting queue conduit"

    a2 <- liftIO $ async $ do
        runKNXStateM logger state ctx $ forever $ runConduit queueConduit
        putStrLn "Queue conduit finished"

    liftIO $ wait a1

    logErrorNS logSourceKNX . pack $ "Socket closed, exiting KNX loop"

handleQueue :: GroupMessage -> KNXStateM ()
handleQueue msg = do
    logDebugNS logSourceKNX . pack $ "Sending message: " <> show msg
    sendGroupMessage msg

handleTelegram :: BS.ByteString -> KNXStateM ()
handleTelegram bs = do
    let msg = parseMessage $ LBS.fromStrict bs
    logDebugNS logSourceKNX $ "Received message: " <> hexWithSpaces (LBS.fromStrict bs)
    case msg of
        Left err -> logErrorNS logSourceKNX . pack $ "Error parsing message: " <> err
        Right msg -> lift $ processTelegram msg

sourceTQueue :: MonadIO m => TQueue a -> ConduitT i a m ()
sourceTQueue queue = forever $ do
    msg <- liftIO $ atomically $ readTQueue queue
    yield msg

recvConduit :: (MonadIO m) => Socket -> ConduitT () BS.ByteString m ()
recvConduit sock = CN.sourceSocket sock .| messageChunks

processTelegram :: IncomingMessage -> KNXCtxM ()
processTelegram msg = do
    logDebugNS logSourceKNX . pack $ "Received message: " <> show msg
    KNXCallback cb' <- asks callback
    liftIO $ cb' msg

composeMessage :: Put -> LBS.ByteString
composeMessage msg = runPut $ do
    let msgLength = LBS.length msg'
    putWord16be $ fromIntegral msgLength
    putLazyByteString msg'

    where
        msg' = runPut msg

sendTelegram :: KNXTelegram -> KNXStateM ()
sendTelegram telegram = do
    sock <- gets sock
    let msg = composeMessage $ B.put telegram
    logDebugNS logSourceKNX $ "Sending: " <> hexWithSpaces msg
    _ <- liftIO $ send sock (LBS.toStrict msg)

    -- Echo the message back to the callback
    lift $ case apci $ apdu telegram of
        ACPIGroupValueRead      -> processTelegram $ IncomingGroupValueRead (dstField telegram)
        ACPIGroupValueResponse  -> processTelegram $ IncomingGroupValueResponse (dstField telegram) (payload $ apdu telegram)
        ACPIGroupValueWrite     -> processTelegram $ IncomingGroupValueWrite (dstField telegram) (payload $ apdu telegram)
        _    -> return ()

sendGroupMessage :: GroupMessage -> KNXStateM ()
sendGroupMessage (GroupValueWrite groupAddress dpt) = do
    logDebugNS logSourceKNX . pack $ "Writing " <> show dpt <> " to " <> show groupAddress
    sendTelegram $ composeTelegram ACPIGroupValueWrite groupAddress (Just dpt)

sendGroupMessage (GroupValueRead groupAddress) = do
    logDebugNS logSourceKNX . pack $ "Reading from " <> show groupAddress
    sendTelegram $ composeTelegram ACPIGroupValueRead groupAddress Nothing

sendGroupMessage (GroupValueResponse groupAddress dpt) = do
    logDebugNS logSourceKNX . pack $ "Responding to " <> show groupAddress <> " with " <> show dpt
    sendTelegram $ composeTelegram ACPIGroupValueResponse groupAddress (Just dpt)

sendMessage :: TQueue GroupMessage -> GroupMessage -> IO ()
sendMessage queue msg = atomically $ writeTQueue queue $ msg