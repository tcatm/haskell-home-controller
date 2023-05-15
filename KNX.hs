{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module KNX 
    ( connectKnx
    , disconnectKnx
    , groupWrite
    , runKnxLoop
    , KNXConnection (..)
    , GroupMessage (..)
    , IncomingGroupMessage (..)
    , KNXCallback (..)
    , KNXM (..)
    , runKNX
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
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

eibOpenGroupcon = 0x26

newtype KNXCallback = KNXCallback (IncomingGroupMessage -> IO ())

newtype KNXM a = KNXM { runKNXM :: ReaderT KNXConnection IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runKNX :: KNXConnection -> KNXM a -> IO a
runKNX knx = flip runReaderT knx . runKNXM

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

runKnxLoop :: KNXCallback -> KNXM ()
runKnxLoop cb = do
    liftIO $ putStrLn "Starting KNX loop"
    evalStateT (loop cb) LBS.empty
    where
        loop :: KNXCallback -> StateT LBS.ByteString KNXM ()
        loop cb = do
            knx <- lift $ KNXM ask
            input <- liftIO $ LBS.fromStrict <$> recv (sock knx) 1024  -- liftIO should be used here
            if LBS.null input
                then do
                    liftIO $ putStrLn "Connection closed by the server"
                    return ()
                else do
                    processInput input cb
                    loop cb

processInput :: LBS.ByteString -> KNXCallback -> StateT LBS.ByteString KNXM ()
processInput input cb = do
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
                    Right msg -> let KNXCallback cb' = cb in cb' msg
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

disconnectKnx :: KNXM ()
disconnectKnx = KNXM $ do
    knx <- ask
    liftIO $ close (sock knx)
    return ()

sendTelegram :: Telegram -> KNXM ()
sendTelegram telegram = KNXM $ do
    knx <- ask
    let msg = composeMessage $ B.encode telegram
    -- putStrLn $ "Sending: " ++ hexWithSpaces msg
    _ <- liftIO $ send (sock knx) (LBS.toStrict msg)
    return ()

groupWrite :: GroupMessage -> KNXM ()
groupWrite (GroupMessage groupAddress dpt) = do
    liftIO $ putStrLn $ "Writing " ++ show dpt ++ " to " ++ show groupAddress
    let telegram = Telegram { messageCode = 39
                            , srcField = Nothing
                            , dstField = groupAddress
                            , apdu = APDU { tpci = 0x00
                                          , apci = 0x80
                                          , APDU.payload = encodeDPT dpt
                                          }
                            }
    sendTelegram telegram
    return ()