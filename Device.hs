module Device 
    ( Device (..)
    , processDeviceState
    , performDeviceActions
    , sampleDevice
    ) where

import KNX
import KNXAddress
import DPTs
import Control.Monad
import Data.Binary.Get

data Device s a = Device { runDevice :: s -> IncomingGroupMessage -> (a, s, [GroupMessage]) }

instance Functor (Device s) where
    fmap f device = Device $ \s msg -> 
        let (a, s', msgs) = runDevice device s msg
        in (f a, s', msgs)

instance Applicative (Device s) where
    pure a = Device $ \s _ -> (a, s, [])
    deviceF <*> deviceA = Device $ \s msg ->
        let (f, s', msgsF) = runDevice deviceF s msg
            (a, s'', msgsA) = runDevice deviceA s' msg
        in (f a, s'', msgsF ++ msgsA)

instance Monad (Device s) where
    device >>= f = Device $ \s msg ->
        let (a, s', msgs) = runDevice device s msg
            Device g = f a
            (a', s'', msgs') = g s' msg
        in (a', s'', msgs ++ msgs')

getState :: Device s s
getState = Device $ \s _ -> (s, s, [])

modifyState :: (s -> s) -> Device s ()
modifyState f = Device $ \s _ -> ((), f s, [])

sendMessage :: GroupMessage -> Device s ()
sendMessage msg = Device $ \s _ -> ((), s, [msg])

getInputMessage :: Device s IncomingGroupMessage
getInputMessage = Device $ \s msg -> (msg, s, [])

processDeviceState :: KNXConnection -> IncomingGroupMessage -> (Device Int (), Int) -> IO (Device Int (), Int)
processDeviceState knx msg (device, state) = do
  let (_, newState, outputMessages) = runDevice device state msg
  performDeviceActions knx outputMessages
  when (newState /= state) $ putStrLn $ "New state: " ++ show newState
  return (device, newState)

performDeviceActions :: KNXConnection -> [GroupMessage] -> IO ()
performDeviceActions knx outputMessages = do
    mapM_ (\outputMessage -> do
          putStrLn $ "Output message: " ++ show outputMessage
          -- Send the output messages to the KNX bus
          groupWrite knx outputMessage
          ) outputMessages

sampleDevice :: Device Int ()
sampleDevice = do
    msg <- getInputMessage
    case msg of
      IncomingGroupMessage (GroupAddress 0 1 11) payload -> do
        let EncodedDPT bs short = payload
        let dpt = runGet parseDPT18_1 bs
        modifyState (+1)
        sendMessage $ GroupMessage (GroupAddress 0 1 12) dpt
      _ -> return ()