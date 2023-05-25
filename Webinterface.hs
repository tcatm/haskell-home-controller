{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}

module Webinterface
    ( runWebinterface
    ) where

import           Control.Monad.Logger
import           Control.Monad          (forM_, forever, unless)
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.Trans.State as StateT
import           Data.Aeson             (Value, encode)
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Text              (Text, pack)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Lazy.Char8 as C
import           Yesod
import           Yesod.Static

-- A buffered channel consists of a list of all messages, and a TChan for new messages.
data BufferedTChan a = BufferedTChan (TVar [a]) (TChan a)

newBufferedTChanIO :: IO (BufferedTChan a)
newBufferedTChanIO = do
  history <- newTVarIO []
  chan <- newBroadcastTChanIO
  return (BufferedTChan history chan)

-- Write a value to the BufferedTChan. The value is added to the end of the list and the TChan.
writeBufferedTChan :: BufferedTChan a -> a -> STM ()
writeBufferedTChan (BufferedTChan history chan) a = do
  hist <- readTVar history
  writeTVar history (hist ++ [a])
  writeTChan chan a

-- Duplicate the BufferedTChan. The new BufferedTChan starts with all values in the list, 
-- followed by any new values added to the TChan.
dupBufferedTChan :: BufferedTChan a -> STM ([a], TChan a)
dupBufferedTChan (BufferedTChan history origChan) = do
  hist <- readTVar history
  newChan <- dupTChan origChan
  return (hist, newChan)

mergeDupBufferedTChan :: ([a], TChan a) -> STM (TChan a)
mergeDupBufferedTChan (hist, origChan) = do
  replayChan <- newTChan
  forM_ hist $ writeTChan replayChan
  return replayChan

-- This will be called instead of readTChan in the event loop
readEitherTChan :: TChan a -> TChan a -> STM a
readEitherTChan chan1 chan2 = do
  empty1 <- isEmptyTChan chan1
  if empty1
    then readTChan chan2
    else readTChan chan1

-- Merge two TChans. The resulting TChan first provides all values from the first TChan, 
-- then all values from the second.
tchanMerge :: TChan a -> TChan a -> IO (TChan a)
tchanMerge chan1 chan2 = do
  mergedChan <- atomically $ newTChan
  let loop c1 c2 = atomically $ do
        empty <- isEmptyTChan c1
        if empty
          then return $ Just c2
          else do
            a <- readTChan c1
            writeTChan mergedChan a
            return $ Just c1
  _ <- forkIO $ do
    currChan <- loop chan1 chan2
    case currChan of
      Nothing -> return ()
      Just c  -> loop c mergedChan >> return ()
  return mergedChan

data App = App
    { chan :: BufferedTChan Entry
    , getStatic :: Static
    }

data Entry = Entry
    { entryId   :: Integer
    , entryData :: LBS.ByteString
    }

mkYesod "App" [parseRoutesNoCheck|
/stream StreamR GET
/ StaticR Static getStatic
|]

instance Yesod App

producer :: TQueue Value -> BufferedTChan Entry -> StateT Integer IO ()
producer inputQueue chan = loop
    where
        loop :: StateT Integer IO ()
        loop = do
            value <- liftIO $ atomically $ readTQueue inputQueue
            id <- StateT.get
            let entry = Entry
                    { entryId = id
                    , entryData = encode value
                    }
            liftIO $ atomically $ writeBufferedTChan chan $ entry
            modify (+1)
            loop

getStreamR :: Handler TypedContent
getStreamR = do
    App {..} <- getYesod

    (chanHistory, origChan) <- liftIO $ atomically $ dupBufferedTChan chan
    replayChan <- liftIO $ atomically $ mergeDupBufferedTChan (chanHistory, origChan)

    respondSource "text/event-stream" $ do
        sendFlush
        let loop = do
            entry <- liftIO $ atomically $ readEitherTChan replayChan origChan
            let idLBS = C.pack $ show $ entryId entry
            let event = "id: " <> idLBS <> "\ndata: " <> entryData entry <> "\n\n"
            sendChunkLBS event
            sendFlush
            loop
        loop

webinterface :: TQueue Value -> IO ()
webinterface inputQueue = do
    chan <- newBufferedTChanIO
    forkIO $ evalStateT (producer inputQueue chan) 0
    getStatic@(Static settings) <- static "web/"
    warp 3000 App {..}

runWebinterface :: TQueue Value -> LoggingT IO ()
runWebinterface = liftIO . webinterface