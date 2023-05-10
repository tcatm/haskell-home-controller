module DPTs
    ( DPT (..)
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int

data DPT = DPT1 Bool -- short
         | DPT2 (Bool, Bool) --short
         | DPT3 Int -- 4bit, short
         | DPT4 Char
         | DPT5 Word8
         | DPT6 Int8
         | DPT7 Word16
         | DPT8 Int16
         | DPT9 Float
         | DPT10 (Word, Word, Word, Word) -- 1st Word8 is DayOfWeek (1..7, 0 = no day)
         | DPT11 (Word, Word, Word)
         | DPT12 Word32
         | DPT13 Int32
         | DPT14 Float
         | DPT15 Word32
         | DPT16 String
            deriving (Eq, Show)
