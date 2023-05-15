module DeviceRunner
    ( DeviceInput (..)
    , DeviceState (..)
    , Continuation (..)
    , processDeviceInput
    , performDeviceActions
    , startDevice
    , runDevicesLoop
    ) where

import KNX hiding (groupWrite)
import qualified KNX as KNX
import DPTs
import Device
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Binary.Get
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import System.Console.Pretty

data DeviceInput = KNXGroupMessage IncomingGroupMessage | TimerEvent UTCTime deriving (Show)

runDevicesLoop :: KNXConnection -> [Device] -> MVar DeviceInput -> IO ()
runDevicesLoop knx devices deviceInput = do
  continuationsWithState <- mapM (\device -> startDevice knx deviceInput device) devices
  deviceLoop knx continuationsWithState deviceInput

deviceLoop :: KNXConnection -> [([Continuation], DeviceState)] -> MVar DeviceInput -> IO ()
deviceLoop knx continuationsWithState deviceInput = do
  msg <- takeMVar deviceInput
  putStrLn $ "Received DeviceInput " ++ show msg
  
  -- apply msg to all devices
  continuationsWithState' <- mapM (\(continuations, state) -> processDeviceInput knx deviceInput msg (continuations, state)) continuationsWithState

  deviceLoop knx continuationsWithState' deviceInput

performContinuationWithInput :: KNXConnection -> MVar DeviceInput -> DeviceState -> Continuation -> DeviceInput -> IO ([Continuation], DeviceState)
performContinuationWithInput knx deviceInput state c@(GroupReadContinuation ga parser cont) (KNXGroupMessage msg) = do
    time <- getZonedTime
    putStrLn $ color Green $ "Performing " ++ show c
    putStrLn $ color Green $ "    Message: " ++ show msg

    let dpt = runGet parser (encodedDPT $ payload msg)
    putStrLn $ color Green $ "    DPT: " ++ show dpt

    let (a, s, actions) = runDeviceM (cont dpt) (time, state)

    performDeviceActions knx deviceInput s actions

performContinuation :: KNXConnection -> MVar DeviceInput -> DeviceState -> Continuation -> IO ([Continuation], DeviceState)
performContinuation knx deviceInput state c@(Continuation device) = do
    time <- getZonedTime
    putStrLn $ color Green $ "Performing " ++ show c

    let (a, s, actions) = runDeviceM device (time, state)

    performDeviceActions knx deviceInput s actions

performContinuation knx deviceInput state c@(ScheduledContinuation _ device) = do
    time <- getZonedTime
    putStrLn $ color Green $ "Performing " ++ show c

    let (a, s, actions) = runDeviceM device (time, state)

    performDeviceActions knx deviceInput s actions
    
-- | The 'startDevice' function starts a device. It does not process any inputs and creates the initial state.
startDevice :: KNXConnection -> MVar DeviceInput -> Device -> IO ([Continuation], DeviceState)
startDevice knx deviceInput device = do
    performContinuation knx deviceInput (initialDeviceState device) (Continuation $ entryPoint device)

processDeviceInput :: KNXConnection -> MVar DeviceInput -> DeviceInput -> ([Continuation], DeviceState) -> IO ([Continuation], DeviceState)
processDeviceInput knx deviceInput (KNXGroupMessage msg) (continuations, state) = do
    time <- getZonedTime
    let groupAddress = incomingGroupAddress msg
    -- partition continuations into those that match the incoming message and those that don't

    let (a, b) = partition (\c -> case c of
                                GroupReadContinuation ga _ _ -> ga == groupAddress
                                _ -> False
                            ) continuations

    -- for each matching continuation, run the continuation and add the resulting continuation to the list of continuations
    -- basically, fold them

    let f (continuations, state) continuation = do
            (continuations', state') <- performContinuationWithInput knx deviceInput state continuation (KNXGroupMessage msg)
            return (continuations' ++ continuations, state')
        
    (continuations', state') <- foldM f ([], state) a

    return (continuations' ++ b, state')

processDeviceInput knx deviceInput (TimerEvent time) (continuations, state) = do
    let (a, b) = partition (\c -> case c of
                                ScheduledContinuation t _ -> t <= time
                                _ -> False
                            ) continuations

    let f (continuations, state) continuation = do
            (continuations', state') <- performContinuation knx deviceInput state continuation
            return (continuations' ++ continuations, state')
        
    (continuations', state') <- foldM f ([], state) a

    return (continuations' ++ b, state')

performDeviceActions :: KNXConnection -> MVar DeviceInput -> DeviceState -> [Action] -> IO ([Continuation], DeviceState)
performDeviceActions knx deviceInput state actions = do
    let f (devices, state) action = do
            (maybeDevice, state') <- performDeviceAction knx deviceInput state action
            return $ case maybeDevice of
                Just device -> (device:devices, state')
                Nothing -> (devices, state')

    putStrLn $ color Green $ "    Actions: " ++ show actions

    (continuations, state') <- foldM f ([], state) actions

    putStrLn $ color Green $ "    Final state: " ++ show state'
    putStrLn $ color Green $ "    Continuations: " ++ show continuations

    return (continuations, state')

performDeviceAction :: KNXConnection -> MVar DeviceInput -> DeviceState -> Action -> IO (Maybe Continuation, DeviceState)
performDeviceAction knx deviceInput state (Log msg) = do
    putStrLn $ color Yellow $ "    " ++ msg
    return (Nothing, state)

performDeviceAction knx deviceInput state (GroupWrite ga dpt) = do
    putStrLn $ color Green $ "    Writing " ++ show dpt ++ " to " ++ show ga
    runKNX knx (KNX.groupWrite $ GroupMessage ga dpt)
    return (Nothing, state)

performDeviceAction knx deviceInput state (Defer continuation) = do
    putStrLn $ color Green $ "    Deferring continuation: " ++ show continuation
    case continuation of
        ScheduledContinuation time device -> do
            putStrLn $ color Green $ "        Scheduled continuation: " ++ show time
            forkIO $ do
                currentTime <- getCurrentTime
                let delay = time `diffUTCTime` currentTime
                threadDelay $ ceiling $ 1000000 * delay
                putMVar deviceInput $ TimerEvent time

            return ()

        GroupReadContinuation ga parser cont -> do
            putStrLn $ color Green $ "        GroupReadContinuation: " ++ show ga
            
    return (Just continuation, state)