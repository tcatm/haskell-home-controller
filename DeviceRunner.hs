module DeviceRunner
    ( DeviceInput (..)
    , Continuation (..)
    , runDevices
    ) where

import KNX hiding (groupWrite)
import qualified KNX as KNX
import DPTs
import Device
import Control.Monad
import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Binary.Get
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import System.Console.Pretty

data DeviceInput = StartDevice | KNXGroupMessage IncomingGroupMessage | TimerEvent UTCTime deriving (Show)

type DeviceRunnerT = ReaderT (MVar DeviceInput) KNXM

runDevices :: [SomeDevice] -> MVar DeviceInput -> KNXM ()
runDevices devices deviceInput = 
    runReaderT (deviceRunner devices) deviceInput

deviceRunner :: [SomeDevice] -> DeviceRunnerT ()
deviceRunner devices = do
    mVar <- ask
    devices' <- mapDevices StartDevice devices

    let loop devices'' = do
          input <- liftIO $ takeMVar mVar
          devices''' <- mapDevices input devices''
          loop devices'''

    loop devices'        

mapDevices :: DeviceInput -> [SomeDevice] -> DeviceRunnerT [SomeDevice]
mapDevices input = mapM (\(SomeDevice d) -> SomeDevice <$> processDeviceInput input d)

performAction :: (Show s) => s -> Continuation s -> DeviceM s () -> DeviceRunnerT ([Continuation s], s)
performAction state c device = do
    time <- liftIO $ getZonedTime
    liftIO $ putStrLn $ color Green $ "Performing " ++ show c

    let (_, state', actions) = runDeviceM device (time, state)

    liftIO $ putStrLn $ color Green $ "    Actions: " ++ show actions

    (continuations, state'') <- performDeviceActions state' actions

    liftIO $ putStrLn $ color Green $ "    Final state: " ++ show state''
    liftIO $ putStrLn $ color Green $ "    Continuations: " ++ show continuations

    return (continuations, state')

performContinuationWithInput :: (Show s) => DeviceInput -> s  -> Continuation s -> DeviceRunnerT ([Continuation s], s)
performContinuationWithInput (KNXGroupMessage msg) state c@(GroupReadContinuation ga parser cont) = do
    let dpt = runGet parser (encodedDPT $ payload msg)
    liftIO $ putStrLn $ color Green $ "    DPT: " ++ show dpt

    performAction state c (cont dpt)

performContinuation :: (Show s) => s -> Continuation s -> DeviceRunnerT ([Continuation s], s)
performContinuation state c@(Continuation device) =
    performAction state c device

performContinuation state c@(ScheduledContinuation _ device) =
    performAction state c device

processDeviceInput :: (Show s) => DeviceInput -> Device s -> DeviceRunnerT (Device s)
processDeviceInput input device = do
    let continuations = deviceContinuations device
    let state = deviceState device

    let (filterF, performF) = case input of
            StartDevice ->
                ((\c -> case c of
                            Continuation _ -> True
                            _ -> False),
                performContinuation )
            KNXGroupMessage msg ->
                (let groupAddress = incomingGroupAddress msg in
                (\c -> case c of
                            GroupReadContinuation ga _ _ -> ga == groupAddress
                            _ -> False),
                performContinuationWithInput (KNXGroupMessage msg))
            TimerEvent time ->
                ((\c -> case c of
                            ScheduledContinuation t _ -> t <= time
                            _ -> False),
                performContinuation)

    let (matchingContinuations, otherContinuations) = partition filterF continuations

    let f (continuations, state) continuation = do
            (continuations', state') <- performF state continuation
            return (continuations' ++ continuations, state')

    -- If any devices are run, print a message
    when (not $ null matchingContinuations) $
        liftIO $ putStrLn $ color Blue $ "Processing " ++ show input ++ " for " ++ deviceName device

    (continuations', state') <- foldM f ([], state) matchingContinuations

    return device { deviceContinuations = continuations' ++ otherContinuations, deviceState = state' }

performDeviceActions :: (Show s) => s -> [Action s] -> DeviceRunnerT ([Continuation s], s)
performDeviceActions state actions = do
    let f (accumulatedContinuations, state) action = do
            (maybeContinuation, state') <- performDeviceAction state action
            return $ case maybeContinuation of
                Just continuation -> (continuation:accumulatedContinuations, state')
                Nothing -> (accumulatedContinuations, state')

    (continuations, state') <- foldM f ([], state) actions

    return (continuations, state')

performDeviceAction :: s -> Action s -> DeviceRunnerT (Maybe (Continuation s), s)
performDeviceAction state (Log msg) = do
    liftIO $ putStrLn $ color Yellow $ msg
    return (Nothing, state)

performDeviceAction state (GroupWrite ga dpt) = do
    liftIO $ putStrLn $ color Magenta $ "    GroupMessage " ++ show dpt ++ " to " ++ show ga
    lift $ KNX.groupWrite $ GroupMessage ga dpt
    return (Nothing, state)

performDeviceAction state (Defer continuation) = do
    liftIO $ putStrLn $ color Magenta $ "    Deferring continuation: " ++ show continuation
    case continuation of
        Continuation device -> do
            return ()

        GroupReadContinuation ga _ _ -> do
            -- TODO: Maybe trigger a group read on KNX?
            return ()

        ScheduledContinuation time device -> do
            mVar <- ask
            liftIO $ forkIO $ do
                currentTime <- getCurrentTime
                let delay = time `diffUTCTime` currentTime
                threadDelay $ ceiling $ 1000000 * delay
                putMVar mVar $ TimerEvent time

            return ()

    return (Just continuation, state)