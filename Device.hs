module Device 
    ( Device (..)
    , DeviceInput (..)
    , DeviceState (..)
    , Continuation (..)
    , processDeviceInput
    , performDeviceActions
    , startDevice
    , modifyState
    , debug
    , groupWrite
    , groupRead
    , schedule
    , getTime
    ) where

import KNX hiding (groupWrite)
import qualified KNX as KNX
import KNXAddress
import DPTs
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Binary.Get
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Time.Clock
import System.Console.Pretty

data Continuation = Continuation (Device DeviceState ()) -- Used for starting a device
                  | GroupReadContinuation GroupAddress (Get DPT) (DPT -> Device DeviceState ())
                  | ScheduledContinuation UTCTime (Device DeviceState ())

instance Show Continuation where
    show (Continuation _) = "Continuation"
    show (GroupReadContinuation ga _ _) = "GroupReadContinuation " ++ show ga
    show (ScheduledContinuation time _) = "ScheduledContinuation " ++ show time

data DeviceInput = KNXGroupMessage IncomingGroupMessage | TimerEvent UTCTime deriving (Show)

data Action = GroupWrite GroupAddress DPT
            | Defer Continuation
            | Log String

instance Show Action where
    show (GroupWrite ga dpt) = "GroupWrite " ++ show ga ++ " " ++ show dpt
    show (Defer c) = "Defer " ++ show c
    show (Log msg) = "Log " ++ msg

data DeviceState = DeviceState 
    { counter :: Int
    } deriving (Show)

initialDeviceState :: DeviceState
initialDeviceState = DeviceState
    { counter = 0
    }

data Device s a = Device { runDevice :: (UTCTime, DeviceState) -> (a, DeviceState, [Action]) }

instance Show (Device s a) where
    show _ = "Device <function>"

instance Functor (Device s) where
    fmap f device = Device $ \(time, s) -> 
        let (a, s', actions) = runDevice device (time, s)
        in (f a, s', actions)

instance Applicative (Device s) where
    pure a = Device $ \(time, s) -> (a, s, [])

    deviceF <*> deviceA = Device $ \(time, s) ->
        let (f, s', actions) = runDevice deviceF (time, s)
            (a, s'', actions') = runDevice deviceA (time, s')
        in (f a, s'', actions ++ actions')

instance Monad (Device s) where
    device >>= f = Device $ \(time, s) ->
        let (a, s', actions) = runDevice device (time, s)
            (b, s'', actions') = runDevice (f a) (time, s')
        in (b, s'', actions ++ actions')

modifyState :: (DeviceState -> DeviceState) -> Device s ()
modifyState f = Device $ \(time, s) -> ((), f s, [])

debug :: String -> Device s ()
debug msg = Device $ \(time, s) -> ((), s, [Log msg])

groupWrite :: GroupAddress -> DPT -> Device s ()
groupWrite ga dpt = Device $ \(time, s) -> ((), s, [action])
    where
        action = GroupWrite ga dpt

groupRead :: GroupAddress -> (Get DPT) -> (DPT -> Device DeviceState ()) -> Device DeviceState ()
groupRead ga parser cont = Device $ \(time, s) -> ((), s, [Defer $ GroupReadContinuation ga parser cont])

schedule :: UTCTime -> Device DeviceState () -> Device s ()
schedule time device = Device $ \(_, s) -> ((), s, [action])
    where
        action = Defer $ ScheduledContinuation time device

getTime :: Device s UTCTime
getTime = Device $ \(time, s) -> (time, s, [])

performContinuationWithInput :: KNXConnection -> MVar DeviceInput -> DeviceState -> Continuation -> DeviceInput -> IO ([Continuation], DeviceState)
performContinuationWithInput knx mVar state c@(GroupReadContinuation ga parser cont) (KNXGroupMessage msg) = do
    time <- getCurrentTime
    putStrLn $ color Green $ "Performing " ++ show c
    putStrLn $ color Green $ "    Message: " ++ show msg

    let dpt = runGet parser (encodedDPT $ payload msg)
    putStrLn $ color Green $ "    DPT: " ++ show dpt

    let (a, s, actions) = runDevice (cont dpt) (time, state)

    performDeviceActions knx mVar s actions

performContinuation :: KNXConnection -> MVar DeviceInput -> DeviceState -> Continuation -> IO ([Continuation], DeviceState)
performContinuation knx mVar state c@(Continuation device) = do
    time <- getCurrentTime
    putStrLn $ color Green $ "Performing " ++ show c

    let (a, s, actions) = runDevice device (time, state)

    performDeviceActions knx mVar s actions

performContinuation knx mVar state c@(ScheduledContinuation _ device) = do
    time <- getCurrentTime
    putStrLn $ color Green $ "Performing " ++ show c

    let (a, s, actions) = runDevice device (time, state)

    performDeviceActions knx mVar s actions
    
-- | The 'startDevice' function starts a device. It does not process any inputs and creates the initial state.
startDevice :: KNXConnection -> MVar DeviceInput -> Device DeviceState () -> IO ([Continuation], DeviceState)
startDevice knx mVar device = do
    performContinuation knx mVar initialDeviceState (Continuation device)

processDeviceInput :: KNXConnection -> MVar DeviceInput -> DeviceInput -> ([Continuation], DeviceState) -> IO ([Continuation], DeviceState)
processDeviceInput knx mVar (KNXGroupMessage msg) (continuations, state) = do
    time <- getCurrentTime
    let groupAddress = incomingGroupAddress msg
    -- partition continuations into those that match the incoming message and those that don't

    let (a, b) = partition (\c -> case c of
                                GroupReadContinuation ga _ _ -> ga == groupAddress
                                _ -> False
                            ) continuations

    -- for each matching continuation, run the continuation and add the resulting continuation to the list of continuations
    -- basically, fold them

    let f (continuations, state) continuation = do
            (continuations', state') <- performContinuationWithInput knx mVar state continuation (KNXGroupMessage msg)
            return (continuations' ++ continuations, state')
        
    (continuations', state') <- foldM f ([], state) a

    return (continuations' ++ b, state')

processDeviceInput knx mVar (TimerEvent time) (continuations, state) = do
    let (a, b) = partition (\c -> case c of
                                ScheduledContinuation t _ -> t <= time
                                _ -> False
                            ) continuations

    let f (continuations, state) continuation = do
            (continuations', state') <- performContinuation knx mVar state continuation
            return (continuations' ++ continuations, state')
        
    (continuations', state') <- foldM f ([], state) a

    return (continuations' ++ b, state')

performDeviceActions :: KNXConnection -> MVar DeviceInput -> DeviceState -> [Action] -> IO ([Continuation], DeviceState)
performDeviceActions knx mVar state actions = do
    let f (devices, state) action = do
            (maybeDevice, state') <- performDeviceAction knx mVar state action
            return $ case maybeDevice of
                Just device -> (device:devices, state')
                Nothing -> (devices, state')

    putStrLn $ color Green $ "    Actions: " ++ show actions

    (continuations, state') <- foldM f ([], state) actions

    putStrLn $ color Green $ "    Final state: " ++ show state'
    putStrLn $ color Green $ "    Continuations: " ++ show continuations

    return (continuations, state')

performDeviceAction :: KNXConnection -> MVar DeviceInput -> DeviceState -> Action -> IO (Maybe Continuation, DeviceState)
performDeviceAction knx mVar state (Log msg) = do
    putStrLn $ color Yellow $ "    " ++ msg
    return (Nothing, state)

performDeviceAction knx mVar state (GroupWrite ga dpt) = do
    putStrLn $ color Green $ "    Writing " ++ show dpt ++ " to " ++ show ga
    KNX.groupWrite knx $ GroupMessage ga dpt
    return (Nothing, state)

performDeviceAction knx mVar state (Defer continuation) = do
    putStrLn $ color Green $ "    Deferring continuation: " ++ show continuation
    case continuation of
        ScheduledContinuation time device -> do
            putStrLn $ color Green $ "        Scheduled continuation: " ++ show time
            forkIO $ do
                currentTime <- getCurrentTime
                let delay = time `diffUTCTime` currentTime
                threadDelay $ ceiling $ 1000000 * delay
                putMVar mVar $ TimerEvent time

            return ()

        GroupReadContinuation ga parser cont -> do
            putStrLn $ color Green $ "        GroupReadContinuation: " ++ show ga
            
    return (Just continuation, state)

-- Scene multiplexer
-- sceneMultiplexer :: GroupAddress -> Int -> GroupAddress -> Device DeviceState ()
-- sceneMultiplexer inputAddr offset outputAddr = do
--     a <- waitFor inputAddr parseDPT18_1

--     case a of
--         Just (DPT18_1 (False, val_a)) -> do
--             groupWrite outputAddr (DPT18_1 (False, (val_a `mod` 4) + offset))
--         _ -> return ()
