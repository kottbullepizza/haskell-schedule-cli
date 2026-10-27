module Timeblock
    ( Timeblock(..)
    , startsAt
    , endsAt
    ) where

import Data.Time

data Timeblock = Timeblock
    { timeblockId          :: Int
    , timeblockTitle       :: String
    , timeblockDescription :: String
    , timeblockStart       :: UTCTime
    , timeblockEnd         :: UTCTime
    }
    deriving (Show, Eq)

-- AN EXQUISITE COMMENT THAT DESCRIBES THE FABULOUS FUNCTIONS BELOW --

startsAt :: TimeZone -> LocalTime -> Timeblock -> Bool
startsAt timeZone time block =
    time == utcToLocalTime timeZone (timeblockStart block)

endsAt :: TimeZone -> LocalTime -> Timeblock -> Bool
endsAt timeZone time block =
    time == utcToLocalTime timeZone (timeblockEnd block)