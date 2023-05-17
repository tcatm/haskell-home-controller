module KNXDatatypes
    ( KNXTimeOfDay (..)
    , putKNXFloat16
    , getKNXFloat16
    ) where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar

import GHC.Float

import Debug.Trace

data KNXTimeOfDay = KNXTimeOfDay
    { knxWeekDay :: Maybe DayOfWeek
    , knxTimeOfDay :: TimeOfDay
    } deriving (Eq, Show)

instance Binary KNXTimeOfDay where
    put (KNXTimeOfDay weekDay timeOfDay) = do
        let weekDay' = fromIntegral $ case weekDay of
                Nothing -> 0x00
                Just dow -> fromEnum dow
            hour = fromIntegral $ todHour timeOfDay
            min = fromIntegral $ todMin timeOfDay
            sec = round $ todSec timeOfDay

        putWord8 $ weekDay' `shiftL` 5 .|. hour
        putWord8 $ min
        putWord8 $ sec

    get = do
        b1 <- getWord8
        b2 <- getWord8
        b3 <- getWord8

        let hour = fromIntegral $ b1 .&. 0x1F
            weekDay' = case b1 `shiftR` 5 of
                0x00 -> Nothing
                dow -> Just $ toEnum $ fromIntegral dow
            min = fromIntegral b2
            sec = fromIntegral b3
        return $ KNXTimeOfDay weekDay' $ TimeOfDay hour min sec

putKNXFloat16 :: Double -> Put
putKNXFloat16 v =
    if isNaN v
        then putWord16be 0x7FFF
    else   
        let v' = v * 100
            (m, e) = decodeFloat v'
            (m', e') = scaleExponent 42 0 15 $ (fromIntegral m, e)
            mantissaInt = round m' :: Int16
            mantissaWord = fromIntegral $ max (-2048) (min 2046 mantissaInt) :: Word16
            mantissaBits = mantissaWord .&. 0x87FF
            exponentBits = fromIntegral $ (e' `shiftL` 11) .&. 0x7800
        in  trace ("v:  " ++ show (word64ToBits $ castDoubleToWord64 v))
            trace ("v': " ++ show (word64ToBits $ castDoubleToWord64 v'))
            trace ("m': " ++ show (word64ToBits $ castDoubleToWord64 m'))
            trace ("w:  " ++ show (word16ToBits $ fromIntegral mantissaInt))
            trace ("e': " ++ show e')
            trace ("putKNXFloat16: " ++ show v ++ " -> m: "
                    ++ word16ToBits mantissaBits ++ ", e: "
                    ++ word16ToBits exponentBits) $
            putWord16be . fromIntegral $ exponentBits .|. mantissaBits
        where
            scaleExponent :: Int -> Int -> Int -> (Double, Int) -> (Double, Int)
            scaleExponent offset low high (m, e) = 
                let s' = e - e' + offset
                    m' = scaleFloat (s' - offset) m
                in trace ("scaleExponent: " ++ show (m, e) ++ " -> " ++ show (m', e'))
                    (m', e')
                where
                    e' = case () of
                        _ | e + offset < low -> low
                          | e + offset > high -> high
                          | otherwise -> e + offset
                          
            word16ToBits :: Word16 -> String
            word16ToBits w = let s = concatMap (\i -> if w .&. (1 `shiftL` i) /= 0 then "1" else "0") [15,14..0]
                            in take 1 s ++ " " ++ take 4 (drop 1 s) ++ " " ++ take 11 (drop 5 s)

            word64ToBits :: Word64 -> String
            word64ToBits w = let s = concatMap (\i -> if w .&. (1 `shiftL` i) /= 0 then "1" else "0") [63,62..0]
                            in take 1 s ++ " " ++ take 8 (drop 1 s) ++ " " ++ take 52 (drop 9 s)

getKNXFloat16 :: Get Double
getKNXFloat16 = do
    mantissaExponent <- getWord16be
    let mantissaRaw = mantissaExponent .&. 0x07FF
        mantissa = if negative
            then - (fromIntegral $ complement (mantissaRaw .|. 0xF800)) - 1
            else fromIntegral $ mantissaRaw
        exponent = fromIntegral $ (mantissaExponent .&. 0x7800) `shiftR` 11
        negative = mantissaExponent .&. 0x8000 /= 0
    if mantissaExponent == 0x7FFF
        then return $ 0.0 / 0.0
        else return $ 0.01 * encodeFloat mantissa exponent