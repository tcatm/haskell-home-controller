module DeviceRunner
    ( DeviceInput (..)
    , DeviceState (..)
    , Continuation (..)
    , runDevices
    ) where

import KNX hiding (groupWrite)
import qualified KNX as KNX
import DPTs
import Device
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans.State
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

runDevices :: [Device] -> MVar DeviceInput -> KNXM ()
runDevices devices deviceInput = 
    runReaderT (deviceRunner devices) deviceInput

deviceRunner :: [Device] -> DeviceRunnerT ()
deviceRunner devices = do
    -- First, perform all unconditional continuations
    devices' <- mapM (processDeviceInput StartDevice) devices

    -- Then, wait for input and perform continuations based on that input in a loop
    deviceLoop devices'

deviceLoop :: [Device] -> DeviceRunnerT ()
deviceLoop devices = do
    mVar <- ask
    msg <- liftIO $ takeMVar mVar
    liftIO $ putStrLn $ "Received DeviceInput " ++ show msg

    -- Process the input for each device
    devices' <- mapM (processDeviceInput msg) devices

    deviceLoop devices'


performAction :: DeviceState -> Continuation -> DeviceM DeviceState () -> DeviceRunnerT ([Continuation], DeviceState)
performAction state c device =  do
    time <- liftIO $ getZonedTime
    liftIO $ putStrLn $ color Green $ "Performing " ++ show c

    let (_, state', actions) = runDeviceM device (time, state)

    liftIO $ putStrLn $ color Green $ "    Actions: " ++ show actions

    (continuations, state'') <- performDeviceActions state' actions

    liftIO $ putStrLn $ color Green $ "    Final state: " ++ show state''
    liftIO $ putStrLn $ color Green $ "    Continuations: " ++ show continuations

    return (continuations, state')

performContinuationWithInput :: DeviceInput -> DeviceState  -> Continuation -> DeviceRunnerT ([Continuation], DeviceState)
performContinuationWithInput (KNXGroupMessage msg) state c@(GroupReadContinuation ga parser cont) = do
    let dpt = runGet parser (encodedDPT $ payload msg)
    liftIO $ putStrLn $ color Green $ "    DPT: " ++ show dpt

    performAction state c (cont dpt)

performContinuation :: DeviceState -> Continuation -> DeviceRunnerT ([Continuation], DeviceState)
performContinuation state c@(Continuation device) =
    performAction state c device

performContinuation state c@(ScheduledContinuation _ device) =
    performAction state c device

processDeviceInput :: DeviceInput -> Device -> DeviceRunnerT Device
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

performDeviceActions :: DeviceState -> [Action] -> DeviceRunnerT ([Continuation], DeviceState)
performDeviceActions state actions = do
    let f (accumulatedContinuations, state) action = do
            (maybeContinuation, state') <- performDeviceAction state action
            return $ case maybeContinuation of
                Just continuation -> (continuation:accumulatedContinuations, state')
                Nothing -> (accumulatedContinuations, state')

    (continuations, state') <- foldM f ([], state) actions

    return (continuations, state')

performDeviceAction :: DeviceState -> Action -> DeviceRunnerT (Maybe Continuation, DeviceState)
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