module Timeblock
    ( Timeblock(..)
    ) where

import Data.Time (UTCTime)

data Timeblock = Timeblock
    { timeblockId          :: Int
    , timeblockTitle       :: String
    , timeblockDescription :: String
    , timeblockStart       :: UTCTime
    , timeblockEnd         :: UTCTime
    }
    deriving (Show, Eq)