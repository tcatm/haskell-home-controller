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

data DeviceState = DeviceState 
    { stateMap :: Map GroupAddress IncomingGroupMessage
    , waitingAddresses :: Map GroupAddress (Get DPT)
    }

instance Show DeviceState where
    show state = "DeviceState { stateMap = " ++ show (stateMap state) ++ ", waitingAddresses = " ++ show (showKeys $ waitingAddresses state) ++ " }"
        where
            showKeys = map (\(k, _) -> k) . Map.toList

newtype Device s a = Device { runDevice :: DeviceState -> (a, DeviceState, [GroupMessage]) }

instance Functor (Device s) where
    fmap f device = Device $ \s -> 
        let (a, s', msgs) = runDevice device s
        in (f a, s', msgs)

instance Applicative (Device s) where
    pure a = Device $ \s -> (a, s, [])

    deviceF <*> deviceA = Device $ \s ->
        let (f, s'@DeviceState{waitingAddresses=waitF, stateMap=stateF}, msgsF) = runDevice deviceF s
            (a, s''@DeviceState{waitingAddresses=waitA, stateMap=stateA}, msgsA) = runDevice deviceA s'
        in (f a, s''{waitingAddresses = Map.union waitF waitA, stateMap = Map.union stateF $ Map.union stateA $ stateMap s}, msgsF ++ msgsA)

instance Monad (Device s) where
    device >>= f = Device $ \s ->
        let (a, s'@DeviceState{waitingAddresses=wait1, stateMap=state1}, msgs) = runDevice device s
            (b, s''@DeviceState{waitingAddresses=wait2, stateMap=state2}, msgs') = runDevice (f a) s'
        in (b, s''{waitingAddresses = Map.union wait1 wait2, stateMap = Map.union state1 $ Map.union state2 $ stateMap s}, msgs ++ msgs')

initialDeviceState :: DeviceState
initialDeviceState = DeviceState Map.empty Map.empty

getState :: Device s DeviceState
getState = Device $ \s -> (s, s, [])

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

            putStrLn $ "Processing device state..."            

            putStrLn $ "    Received message: " ++ show msg
            putStrLn $ "    Current state: " ++ show updatedState

            let (a, finalState, outputMessages) = runDevice device updatedState

            putStrLn $ "    Final state: " ++ show finalState

            unless (null outputMessages) $ do
                putStrLn $ "    Performing device actions..."

                performDeviceActions knx outputMessages
                putStrLn $ "    Device actions completed."

            return finalState
        else
            return newState

performDeviceActions :: KNXConnection -> [GroupMessage] -> IO ()
performDeviceActions knx outputMessages = do
    mapM_ (\outputMessage -> do
          putStrLn $ "Output message: " ++ show outputMessage
          -- Send the output messages to the KNX bus
          KNX.groupWrite knx outputMessage
          ) outputMessages

waitFor :: GroupAddress -> (Get DPT) -> Device DeviceState (Maybe DPT)
waitFor addr parser = Device $ \s ->
    let stateMap' = stateMap s
    in case Map.lookup addr stateMap' of
        Just msg -> (parseMsg msg, s { stateMap = Map.delete addr stateMap' }, [])
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
            c <- waitFor (GroupAddress 0 1 20) parseDPT18_1
            case c of
                Just (DPT18_1 (False, val_c)) -> do
                    groupWrite (GroupAddress 0 1 21) (DPT18_1 (False, val_a + val_b + val_c))
                _ -> return ()
        _ -> return ()