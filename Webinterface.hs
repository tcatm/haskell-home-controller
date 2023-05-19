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
import           Control.Monad          (forM_)
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Text              (Text, pack)
import           Yesod

data App = App
    { chan :: TChan (Maybe Text)
    }

mkYesod "App" [parseRoutes|
/stream StreamR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "PubSub example"
    [whamlet|
        <form method=post>
            <button>Start new background job
    |]

producer :: TChan (Maybe Text) -> IO ()
producer chan = do
        forM_ [1..1000] $ \i -> do
            threadDelay 1000000
            atomically $ writeTChan chan $ Just $ pack $ "Processing item " ++ show i
        atomically $ do
            writeTChan chan $ Just "All done\n"
            writeTChan chan Nothing

getStreamR :: Handler TypedContent
getStreamR = do
    App {..} <- getYesod
    chan <- liftIO $ atomically $ dupTChan chan

    respondSource "text/event-stream" $ do
        let loop = do
                mtext <- liftIO $ atomically $ readTChan chan
                case mtext of
                    Nothing -> return ()
                    Just text -> do
                        let eventDataText = "data: " <> text <> "\n\n"
                        sendChunkText eventDataText
                        sendFlush
                        loop
        loop

webinterface :: IO ()
webinterface = do
    chan <- newBroadcastTChanIO
    forkIO $ producer chan
    warp 3000 App {..}

runWebinterface :: LoggingT IO ()
runWebinterface = liftIO webinterface