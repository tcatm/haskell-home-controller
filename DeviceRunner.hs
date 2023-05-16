module DeviceRunner
    ( DeviceInput (..)
    , Continuation (..)
    , runDevices
    ) where

import KNX hiding (groupWrite)
import qualified KNX as KNX
import DPTs
import Device hiding (gets, modify)
import Control.Monad
import Control.Concurrent
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

data DeviceInput = NoInput | KNXGroupMessage IncomingGroupMessage | TimerEvent TimerId UTCTime deriving (Show)

type DeviceRunnerT = ReaderT (MVar DeviceInput) KNXM
type TimerT = StateT (Map TimerId ThreadId) DeviceRunnerT

runDevices :: [SomeDevice] -> MVar DeviceInput -> KNXM ()
runDevices devices deviceInput = 
    runReaderT (deviceRunner devices) deviceInput

deviceRunner :: [SomeDevice] -> DeviceRunnerT ()
deviceRunner devices = do
    mVar <- ask
    devices' <- mapDevices NoInput devices

    let loop devices'' = do
          input <- liftIO $ takeMVar mVar
          devices''' <- mapDevices input devices''
          loop devices'''

    loop devices'        

mapDevices :: DeviceInput -> [SomeDevice] -> DeviceRunnerT [SomeDevice]
mapDevices input = mapM (\(SomeDevice d) -> SomeDevice <$> processDeviceInput input d)

processDeviceInput :: (Show s) => DeviceInput -> Device s -> DeviceRunnerT (Device s)
processDeviceInput input device = do
    let continuations = deviceContinuations device
    let state = deviceState device

    let filterF = case input of
                        NoInput -> 
                            \c -> case c of
                                Continuation _ -> True
                                _ -> False
                        KNXGroupMessage msg -> 
                            let groupAddress = incomingGroupAddress msg in
                            \c -> case c of
                                GroupReadContinuation ga _ _ -> ga == groupAddress
                                _  -> False
                        TimerEvent timerId time -> 
                            \c -> case c of
                                ScheduledContinuation tId _ _ -> tId == timerId
                                _ -> False

    let (matchingContinuations, otherContinuations) = partition filterF continuations

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
performContinuation (KNXGroupMessage msg) state c@(GroupReadContinuation ga parser cont) = do
    let dpt = runGet parser (encodedDPT $ payload msg)
    liftIO $ putStrLn $ color Green $ "    DPT: " ++ show dpt

    runDeviceWithEffects state c (cont dpt)
    
performContinuation NoInput state c@(Continuation device) =
    runDeviceWithEffects state c device

performContinuation (TimerEvent timerId _) state c@(ScheduledContinuation _ _ device) = do
    modify $ Map.delete timerId
    runDeviceWithEffects state c device

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
    liftIO $ putStrLn $ color Magenta $ "    GroupMessage " ++ show dpt ++ " to " ++ show ga
    lift $ lift $ KNX.groupWrite $ GroupMessage ga dpt
    return Nothing

performDeviceAction (Defer continuation) = do
    liftIO $ putStrLn $ color Magenta $ "    Deferring continuation: " ++ show continuation
    case continuation of
        Continuation device -> do
            return ()

        GroupReadContinuation ga _ _ -> do
            -- TODO: Maybe trigger a group read on KNX?
            return ()

        ScheduledContinuation timerId time device -> do
            mVar <- lift $ ask
            threadId <- liftIO $ forkIO $ do
                currentTime <- getCurrentTime
                let delay = time `diffUTCTime` currentTime
                threadDelay $ ceiling $ 1000000 * delay
                putMVar mVar $ TimerEvent timerId time

            modify $ Map.insert timerId threadId
            return ()

    return $ Just continuation
    
performDeviceAction (CancelTimer timerId) = do
    liftIO $ putStrLn $ color Red $ "    Canceling timer: " ++ show timerId

    threadId <- gets $ Map.lookup timerId
    
    when (isJust threadId) $ do
        modify $ Map.delete timerId
        liftIO $ killThread $ fromJust threadId

    return Nothing