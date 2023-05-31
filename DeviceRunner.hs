{-# LANGUAGE OverloadedStrings #-}

module DeviceRunner
    ( DeviceInput (..)
    , Continuation (..)
    , runDevices
    , logSourceDeviceRunner
    ) where

import Hue.Hue (HueCommand (..))
import KNXMessages
import DPTs
import Device hiding (gets, modify)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Logger
import Data.Binary.Get
import Data.List
import Data.Maybe
import Data.Map (Map)
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map.Strict as Map
import System.Console.Pretty
import Control.Monad.Writer.Lazy
import Data.Aeson
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (unpack)
import GHC.Generics

logSourceDeviceRunner :: LogSource
logSourceDeviceRunner = "DeviceRunner"

data DeviceInput    = StartDevice
                    | KNXIncomingMessage IncomingMessage
                    | TimerEvent TimerId UTCTime
                    deriving (Show)

type DeviceRunnerT = ReaderT DeviceRunnerContext (LoggingT IO)
type WriterDeviceRunnerT = WriterT [Value] DeviceRunnerT
type TimerT = StateT (Map TimerId ThreadId) WriterDeviceRunnerT

data DeviceRunnerContext = DeviceRunnerContext
    { deviceRunnerInputChan :: TChan DeviceInput
    , deviceRunnerKNXQueue :: TQueue GroupMessage
    , deviceRunnerWebQueue :: TQueue Value
    , deviceRunnerHueQueue :: TQueue HueCommand
    }

runDevices :: [Device] -> TChan DeviceInput -> TQueue GroupMessage -> TQueue Value -> TQueue HueCommand -> LoggingT IO ()
runDevices devices deviceInput knxQueue webQueue hueQueue = do
    let ctx = DeviceRunnerContext 
                { deviceRunnerInputChan = deviceInput
                , deviceRunnerKNXQueue = knxQueue
                , deviceRunnerWebQueue = webQueue
                , deviceRunnerHueQueue = hueQueue
                }

    runReaderT (deviceRunner devices) ctx

deviceRunner :: [Device] -> DeviceRunnerT ()
deviceRunner devices = do
    inputChan <- asks deviceRunnerInputChan
    devices' <- mapDevices StartDevice devices

    let loop devices'' = do
          input <- liftIO $ atomically $ readTChan inputChan
          devices''' <- mapDevices input devices''
          loop devices'''

    loop devices'        

mapDevices :: DeviceInput -> [Device] -> DeviceRunnerT [Device]
mapDevices input = mapM (uncurry f) . zip [0..]
    where
        f deviceId (Device d) = Device <$> processDevice deviceId input d

debugDevices :: [Device] -> DeviceRunnerT ()
debugDevices devices = liftIO $ do
    putStrLn $ color Blue "Devices:"
    mapM_ (\(Device d) -> do
        putStrLn $ color Green $ "    " <> deviceName d
        putStrLn $ color Green $ "        Continuations: " <> show (deviceContinuations d)
        ) devices

processDevice :: (Show s, ToJSON s) => Int -> DeviceInput -> Device' s -> DeviceRunnerT (Device' s)
processDevice deviceId input device = do
    (device', log) <- runWriterT $ processDeviceInput input device

    handleLog deviceId device' log

    return device'

handleLog :: (ToJSON s) => Int -> Device' s -> [Value] -> DeviceRunnerT ()
handleLog deviceId device log = do
    unless (null log) $ do
        let log' = object   [ "deviceId" .= deviceId
                            , "deviceName" .= deviceName device
                            , "log" .= log
                            , "state" .= deviceState device
                            , "continuations" .= continuationToJSON device
                            ]
        webQueue <- asks deviceRunnerWebQueue
        liftIO $ atomically $ writeTQueue webQueue log'

continuationToJSON :: Device' s -> [Value]
continuationToJSON device = mapMaybe f $ deviceContinuations device
    where
        f (GroupValueContinuation ga _ _) = Just $ object [ "type" .= ("GroupValue" :: String)
                                                          , "ga" .= ga
                                                          ]
        f (GroupReadContinuation ga _) = Just $ object [ "type" .= ("GroupRead" :: String)
                                                       , "ga" .= ga
                                                       ]
        f (ScheduledContinuation timerId time _) = Just $ object [ "type" .= ("Scheduled" :: String)
                                                                 , "timerId" .= timerId
                                                                 , "time" .= time
                                                                 ]
        f _ = Nothing


filterF :: DeviceInput -> Continuation s -> Bool
filterF input c = case input of
                    StartDevice -> 
                        case c of
                            StartContinuation _ -> True
                            _ -> False
                    KNXIncomingMessage msg -> filterKNXMessage msg c
                    TimerEvent timerId time -> 
                        case c of
                            ScheduledContinuation tId _ _ -> tId == timerId
                            _ -> False

filterKNXMessage :: IncomingMessage -> Continuation s -> Bool
filterKNXMessage (IncomingGroupValueWrite ga _) c =
    case c of
        GroupValueContinuation ga' _ _ -> ga' == ga
        _ -> False

filterKNXMessage (IncomingGroupValueResponse ga _) c =
    case c of
        GroupValueContinuation ga' _ _ -> ga' == ga
        _ -> False

filterKNXMessage (IncomingGroupValueRead ga) c =
    case c of
        GroupReadContinuation ga' _ -> ga' == ga
        _ -> False

processDeviceInput :: (Show s) => DeviceInput -> Device' s -> WriterDeviceRunnerT (Device' s)
processDeviceInput input device = do
    let continuations = deviceContinuations device
    let state = deviceState device

    let (matchingContinuations, otherContinuations) = partition (filterF input) continuations

    let f (continuations, state) continuation = do
            (continuations', state') <- performContinuation input state continuation 
            return (continuations' <> continuations, state')

    if null matchingContinuations
        then return device
        else do
            logInfoNS logSourceDeviceRunner . pack $ color Blue $ "Processing " <> deviceName device <> " with " <> show matchingContinuations
            logInfoNS logSourceDeviceRunner . pack $ color Green $ "    State: " <> show state

            ((continuations', state'), timers) <- runStateT (foldM f ([], state) matchingContinuations) $ deviceTimers device

            let continuations'' = continuations' <> otherContinuations

            -- remove any ScheduledContinuations that have no associated timer in timers
            let continuations''' = filter (\c -> case c of
                                                    ScheduledContinuation timerId _ _ -> Map.member timerId timers
                                                    _ -> True) continuations''

            let device' = device    { deviceContinuations = continuations'''
                                    , deviceState = state'
                                    , deviceTimers = timers
                                    }

            logInfoNS logSourceDeviceRunner . pack $ color Green $ "    Final state: " <> show state'
            logInfoNS logSourceDeviceRunner . pack $ color Blue $ "    Continuations: " <> show continuations'''

            return device'

performContinuation :: (Show s) => DeviceInput -> s -> Continuation s -> TimerT ([Continuation s], s)
performContinuation (KNXIncomingMessage msg) s c = performContinuationKNX msg s c
    
performContinuation _ state c@(StartContinuation device) =
    runDeviceWithEffects state c device

performContinuation (TimerEvent timerId _) state c@(ScheduledContinuation _ _ device) = do
    tell [ object [ "type" .= ("TimerEvent" :: String) , "timerId" .= timerId ] ]
    modify $ Map.delete timerId
    runDeviceWithEffects state c device

performContinuationKNX :: (Show s) => IncomingMessage -> s -> Continuation s -> TimerT ([Continuation s], s)
performContinuationKNX (IncomingGroupValueWrite ga payload) = handleKNXValue payload
performContinuationKNX (IncomingGroupValueResponse ga payload) = handleKNXValue payload
performContinuationKNX (IncomingGroupValueRead ga) = handleKNXread

handleKNXValue :: (Show s) => EncodedDPT -> s -> Continuation s -> TimerT ([Continuation s], s)
handleKNXValue payload state c@(GroupValueContinuation ga parser device) = do
    case runGetOrFail parser (encodedDPT payload) of
        Left (_, _, err) -> do
            logWarnNS logSourceDeviceRunner . pack $ color Red $ "    Error parsing DPT: " <> err
            logWarnNS logSourceDeviceRunner . pack $ color Red $ "    Payload: " <> show payload

            -- Re-queue continuation
            return ([c], state)

        Right (_, _, dpt) -> do
            logInfoNS logSourceDeviceRunner . pack $ color Green $ "    Received " <> show dpt <> " at " <> show ga
            tell [ object [ "type" .= ("KNXIn" :: String) , "ga" .= ga , "dpt" .= dpt ] ]
            runDeviceWithEffects state c (device dpt)

handleKNXread :: (Show s) => s -> Continuation s -> TimerT ([Continuation s], s)
handleKNXread state c@(GroupReadContinuation ga device) = do
    logInfoNS logSourceDeviceRunner . pack $ color Green $ "    Received read request at " <> show ga

    runDeviceWithEffects state c device

runDeviceWithEffects :: (Show s) => s -> Continuation s -> DeviceM s () -> TimerT ([Continuation s], s)
runDeviceWithEffects state c device = do
    time <- liftIO $ getZonedTime

    logInfoNS logSourceDeviceRunner . pack $ color Green $ "    Time: " <> show time

    let (_, state', actions) = runDeviceM device (time, state)

    continuations <- performDeviceActions actions

    return (continuations, state')

performDeviceActions :: (Show s) => [Action s] -> TimerT [Continuation s]
performDeviceActions actions = catMaybes <$> mapM performDeviceAction actions

performDeviceAction :: Action s -> TimerT (Maybe (Continuation s))
performDeviceAction (Log msg) = do
    logInfoNS logSourceDeviceRunner . pack $ color Yellow $ "    " <> msg
    tell [ object [ "type" .= ("Log" :: String) , "message" .= msg ] ]
    return Nothing

performDeviceAction (GroupWrite ga dpt) = do
    logInfoNS logSourceDeviceRunner . pack $ color Magenta $ "    GroupValueWrite " <> show dpt <> " to " <> show ga
    tell [ object [ "type" .= ("KNXOut" :: String) , "ga" .= ga , "dpt" .= dpt ] ]
    lift . lift $ sendKNXMessage $ GroupValueWrite ga dpt
    return Nothing

performDeviceAction (GroupResponse ga dpt) = do
    logInfoNS logSourceDeviceRunner . pack $ color Magenta $ "    GroupValueResponse " <> show dpt <> " to " <> show ga
    lift . lift $ sendKNXMessage $ GroupValueResponse ga dpt
    return Nothing

performDeviceAction (GroupRead ga) = do
    logInfoNS logSourceDeviceRunner . pack $ color Magenta $ "    GroupValueRead from " <> show ga
    lift . lift $ sendKNXMessage $ GroupValueRead ga
    return Nothing

performDeviceAction (HueActivateScene room scene) = do
    logInfoNS logSourceDeviceRunner . pack $ color Magenta $ "    HueActivateScene " <> show scene <> " in " <> show room
    lift . lift $ sendHueMessage $ HueCommandScene room scene
    return Nothing

performDeviceAction (Defer continuation) = do
    logInfoNS logSourceDeviceRunner . pack $ color Magenta $ "    Deferring continuation: " <> show continuation

    case continuation of
        StartContinuation device -> do
            logWarnNS logSourceDeviceRunner . pack $ color Red $ "    Ignored deferred StartContinuation"
            return Nothing

        GroupValueContinuation ga _ _ -> do
            return $ Just continuation

        GroupReadContinuation ga _ -> do
            return $ Just continuation

        ScheduledContinuation timerId time device -> do
            inputChan <- lift . lift $ asks deviceRunnerInputChan
            threadId <- liftIO $ forkIO $ do
                currentTime <- getCurrentTime
                let delay = time `diffUTCTime` currentTime
                threadDelay $ ceiling $ 1000000 * delay
                atomically $ writeTChan inputChan $ TimerEvent timerId time

            modify $ Map.insert timerId threadId
            return $ Just continuation
    
performDeviceAction (CancelTimer timerId) = do
    logInfoNS logSourceDeviceRunner . pack $ color Red $ "    Canceling timer: " ++ show timerId

    threadId <- gets $ Map.lookup timerId
    
    when (isJust threadId) $ do
        modify $ Map.delete timerId
        liftIO $ killThread $ fromJust threadId

    return Nothing

sendKNXMessage :: GroupMessage -> DeviceRunnerT ()
sendKNXMessage msg = do
    queue <- asks deviceRunnerKNXQueue
    liftIO $ atomically $ writeTQueue queue msg

sendHueMessage :: HueCommand -> DeviceRunnerT ()
sendHueMessage msg = do
    queue <- asks deviceRunnerHueQueue
    liftIO $ atomically $ writeTQueue queue msg