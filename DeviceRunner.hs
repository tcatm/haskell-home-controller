module DeviceRunner
    ( DeviceInput (..)
    , Continuation (..)
    , runDevices
    ) where

import KNX hiding (groupWrite, groupRead)
import KNXMessages
import qualified KNX as KNX
import DPTs
import Device hiding (gets, modify)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Binary.Get
import Data.List
import Data.Maybe
import Data.Map (Map)
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map.Strict as Map
import System.Console.Pretty

data DeviceInput = StartDevice | KNXIncomingMessage IncomingMessage | TimerEvent TimerId UTCTime deriving (Show)

type DeviceRunnerT = ReaderT (TQueue DeviceInput) KNXM
type TimerT = StateT (Map TimerId ThreadId) DeviceRunnerT

runDevices :: [Device] -> TQueue DeviceInput -> KNXM ()
runDevices devices deviceInput = 
    runReaderT (deviceRunner devices) deviceInput

deviceRunner :: [Device] -> DeviceRunnerT ()
deviceRunner devices = do
    queue <- ask
    devices' <- mapDevices StartDevice devices

    let loop devices'' = do
          input <- liftIO $ atomically $ readTQueue queue
          devices''' <- mapDevices input devices''
          loop devices'''

    loop devices'        

mapDevices :: DeviceInput -> [Device] -> DeviceRunnerT [Device]
mapDevices input = mapM (\(Device d) -> Device <$> processDeviceInput input d)

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
filterKNXMessage (IncomingWrite msg) c = 
    case c of
        GroupValueContinuation ga _ _ -> ga == incomingGA msg
        _ -> False

filterKNXMessage (IncomingResponse msg) c =
    case c of
        GroupValueContinuation ga _ _ -> ga == incomingGA msg
        _ -> False

filterKNXMessage _ _ = False

processDeviceInput :: (Show s) => DeviceInput -> Device' s -> DeviceRunnerT (Device' s)
processDeviceInput input device = do
    let continuations = deviceContinuations device
    let state = deviceState device

    let (matchingContinuations, otherContinuations) = partition (filterF input) continuations

    let f (continuations, state) continuation = do
            (continuations', state') <- performContinuation input state continuation 
            return (continuations' ++ continuations, state')

    if null matchingContinuations
        then return device
        else do
            liftIO $ putStrLn $ color Blue $ "Processing " ++ show input ++ " for " ++ deviceName device

            ((continuations', state'), timers) <- runStateT (foldM f ([], state) matchingContinuations) $ deviceTimers device

            let continuations'' = continuations' ++ otherContinuations

            -- remove any ScheduledContinuations that have no associated timer in timers
            let continuations''' = filter (\c -> case c of
                                                    ScheduledContinuation timerId _ _ -> Map.member timerId timers
                                                    _ -> True) continuations''

            let device' = device    { deviceContinuations = continuations'''
                                    , deviceState = state'
                                    , deviceTimers = timers
                                    }

            liftIO $ putStrLn $ color Blue $ "Device: " ++ show device'

            return device'

performContinuation :: (Show s) => DeviceInput -> s -> Continuation s -> TimerT ([Continuation s], s)
performContinuation (KNXIncomingMessage msg) s c = performContinuationKNX msg s c
    
performContinuation _ state c@(StartContinuation device) =
    runDeviceWithEffects state c device

performContinuation (TimerEvent timerId _) state c@(ScheduledContinuation _ _ device) = do
    modify $ Map.delete timerId
    runDeviceWithEffects state c device

performContinuationKNX :: (Show s) => IncomingMessage -> s -> Continuation s -> TimerT ([Continuation s], s)
performContinuationKNX (IncomingWrite msg) = handleKNX (igvwPayload msg)
performContinuationKNX (IncomingResponse msg) = handleKNX (igvrPayload msg)

handleKNX :: (Show s) => EncodedDPT -> s -> Continuation s -> TimerT ([Continuation s], s)
handleKNX payload state c@(GroupValueContinuation _ parser device) = do
    case runGetOrFail parser (encodedDPT payload) of
        Left (_, _, err) -> do
            liftIO $ putStrLn $ color Red $ "    Error parsing DPT: " ++ err
            return ([], state)

        Right (_, _, dpt) -> do
            liftIO $ putStrLn $ color Green $ "    DPT: " ++ show dpt

            runDeviceWithEffects state c (device dpt)

runDeviceWithEffects :: (Show s) => s -> Continuation s -> DeviceM s () -> TimerT ([Continuation s], s)
runDeviceWithEffects state c device = do
    time <- liftIO $ getZonedTime
    liftIO $ putStrLn $ color Green $ "    Performing " ++ show c

    let (_, state', actions) = runDeviceM device (time, state)

    liftIO $ putStrLn $ color Green $ "    Final state: " ++ show state'

    continuations <- performDeviceActions actions

    return (continuations, state')

performDeviceActions :: (Show s) => [Action s] -> TimerT [Continuation s]
performDeviceActions actions = catMaybes <$> mapM performDeviceAction actions

performDeviceAction :: Action s -> TimerT (Maybe (Continuation s))
performDeviceAction (Log msg) = do
    liftIO $ putStrLn $ color Yellow $ "    " ++ msg
    return Nothing

performDeviceAction (GroupWrite ga dpt) = do
    liftIO $ putStrLn $ color Magenta $ "    GroupValueWrite " ++ show dpt ++ " to " ++ show ga
    lift $ lift $ KNX.groupWrite $ GroupValueWrite ga dpt
    return Nothing

performDeviceAction (GroupRead ga) = do
    liftIO $ putStrLn $ color Magenta $ "    GroupValueRead from " ++ show ga
    lift $ lift $ KNX.groupRead $ GroupValueRead ga
    return Nothing

performDeviceAction (Defer continuation) = do
    liftIO $ putStrLn $ color Magenta $ "    Deferring continuation: " ++ show continuation

    case continuation of
        StartContinuation device -> do
            liftIO $ putStrLn $ color Red $ "    Ignored deferred StartContinuation"
            return Nothing

        GroupValueContinuation ga _ _ -> do
            return $ Just continuation

        ScheduledContinuation timerId time device -> do
            queue <- lift $ ask
            threadId <- liftIO $ forkIO $ do
                currentTime <- getCurrentTime
                let delay = time `diffUTCTime` currentTime
                threadDelay $ ceiling $ 1000000 * delay
                atomically $ writeTQueue queue $ TimerEvent timerId time

            modify $ Map.insert timerId threadId
            return $ Just continuation
    
performDeviceAction (CancelTimer timerId) = do
    liftIO $ putStrLn $ color Red $ "    Canceling timer: " ++ show timerId

    threadId <- gets $ Map.lookup timerId
    
    when (isJust threadId) $ do
        modify $ Map.delete timerId
        liftIO $ killThread $ fromJust threadId

    return Nothing