module Device 
    ( Device (..)
    , DeviceState
    , processDeviceState
    , performDeviceActions
    , sampleDevice
    , initialDeviceState
    ) where

import KNX hiding (groupWrite)
import qualified KNX as KNX
import KNXAddress
import DPTs
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock
import System.Console.Pretty

data DeviceState = DeviceState 
    { stateMap :: Map GroupAddress IncomingGroupMessage
    , waitingAddresses :: Map GroupAddress (Get DPT)
    , scheduledActions :: [(UTCTime, Device DeviceState ())]
    }

instance Show DeviceState where
    show state = "DeviceState { stateMap = " ++ show (stateMap state) ++ ", waitingAddresses = " ++ show (showKeys $ waitingAddresses state) ++ " }"
        where
            showKeys = map (\(k, _) -> k) . Map.toList

instance Semigroup DeviceState where
    state1 <> state2 = mergeDeviceStates state1 state2

instance Monoid DeviceState where
    mempty = initialDeviceState

initialDeviceState :: DeviceState
initialDeviceState = DeviceState Map.empty Map.empty []

mergeDeviceStates :: DeviceState -> DeviceState -> DeviceState
mergeDeviceStates state1 state2 = DeviceState
    { stateMap = Map.union (stateMap state1) (stateMap state2)
    , waitingAddresses = Map.union (waitingAddresses state1) (waitingAddresses state2)
    , scheduledActions = scheduledActions state1 ++ scheduledActions state2
    }

data Device s a 
    = Device { runDevice :: DeviceState -> (a, DeviceState, [GroupMessage]) }
    | Delay { runDelay :: NominalDiffTime -> Device s a }

instance Functor (Device s) where
    fmap f device = Device $ \s -> 
        let (a, s', msgs) = runDevice device s
        in (f a, s' `mappend` s, msgs)

instance Applicative (Device s) where
    pure a = Device $ \s -> (a, s, [])

    deviceF <*> deviceA = Device $ \s ->
        let (f, s'@DeviceState{waitingAddresses=waitF, stateMap=stateF}, msgsF) = runDevice deviceF s
            (a, s''@DeviceState{waitingAddresses=waitA, stateMap=stateA}, msgsA) = runDevice deviceA s'
        in (f a, s'' `mappend` s', msgsF ++ msgsA)

instance Monad (Device s) where
    device >>= f = Device $ \s ->
        let (a, s'@DeviceState{waitingAddresses=wait1, stateMap=state1}, msgs) = runDevice device s
            (b, s''@DeviceState{waitingAddresses=wait2, stateMap=state2}, msgs') = runDevice (f a) s'
        in (b, s'' `mappend` s', msgs ++ msgs')

modifyState :: (DeviceState -> DeviceState) -> Device s ()
modifyState f = Device $ \s -> ((), f s, [])

groupWrite :: GroupAddress -> DPT -> Device s ()
groupWrite ga dpt = Device $ \s -> ((), s, [msg])
    where
        msg = GroupMessage ga dpt

processDeviceState :: KNXConnection -> IncomingGroupMessage -> (Device DeviceState a, DeviceState) -> IO DeviceState
processDeviceState knx msg (device, state) = do
    let (a, newState, _) = runDevice device state
    let incomingMatch = incomingGroupAddress msg `Map.member` waitingAddresses newState

    if incomingMatch
        then do
            let updatedState = newState { stateMap = Map.insert (incomingGroupAddress msg) msg (stateMap newState) }

            putStrLn $ color Green $ "Processing device state..."       

            putStrLn $ color Green $ "    Received message: " ++ show msg
            putStrLn $ color Green $ "    Current state: " ++ show updatedState

            let (a, finalState, outputMessages) = runDevice device updatedState

            putStrLn $ color Blue $ "    Final state: " ++ show finalState

            unless (null outputMessages) $ do
                putStrLn $ color Green $ "    Performing device actions..."

                performDeviceActions knx outputMessages
                putStrLn $ color Green $  "    Device actions completed."

            return finalState
        else
            return newState

performDeviceActions :: KNXConnection -> [GroupMessage] -> IO ()
performDeviceActions knx outputMessages = do
    mapM_ (\outputMessage -> do
          putStrLn $ color Red $ "Output message: " ++ show outputMessage
          -- Send the output messages to the KNX bus
          KNX.groupWrite knx outputMessage
          ) outputMessages

waitFor :: GroupAddress -> (Get DPT) -> Device DeviceState (Maybe DPT)
waitFor addr parser = Device $ \s ->
    let stateMap' = stateMap s
    in case Map.lookup addr stateMap' of
        Just msg -> (parseMsg msg, s, [])
        Nothing -> (Nothing, s { waitingAddresses = Map.insert addr parser (waitingAddresses s) }, [])
    where
        parseMsg :: IncomingGroupMessage -> Maybe DPT
        parseMsg msg = case runGetOrFail parser (decodeDPT $ payload msg) of
            Left (_, _, err) -> Nothing
            Right (_, _, dpt) -> Just dpt

decodeDPT (EncodedDPT bs _) = bs

sampleDevice :: Device (DeviceState) ()
sampleDevice = do
    a <- waitFor (GroupAddress 0 1 8) parseDPT18_1
    b <- waitFor (GroupAddress 0 1 11) parseDPT18_1

    case (a, b) of
        (Just (DPT18_1 (False, val_a)), Just (DPT18_1 (False, val_b))) -> do
            c <- waitFor (GroupAddress 0 1 (20 + val_a + val_b)) parseDPT18_1
            case c of
                Just (DPT18_1 (False, val_c)) -> do
                    groupWrite (GroupAddress 0 1 21) (DPT18_1 (False, val_a + val_b + val_c))
                _ -> return ()
        _ -> return ()

-- Scene multiplexer
sceneMultiplexer :: GroupAddress -> Int -> GroupAddress -> Device DeviceState ()
sceneMultiplexer inputAddr offset outputAddr = do
    a <- waitFor inputAddr parseDPT18_1

    case a of
        Just (DPT18_1 (False, val_a)) -> do
            groupWrite outputAddr (DPT18_1 (False, (val_a `mod` 4) + offset))
        _ -> return ()