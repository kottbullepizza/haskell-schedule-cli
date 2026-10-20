module DrawTerminal (drawSchedule, testblock) where

import Data.Time
import Data.List (intercalate)

import Timeblock (Timeblock(..))




testblock :: Timeblock
testblock = Timeblock
    { timeblockId          = 0
    , timeblockTitle       = "Testblock"
    , timeblockDescription = "This is a test of a time block"
    , timeblockStart       = strToUtc "2026-10-19 09:30:00"
    , timeblockEnd         = strToUtc "2026-10-19 10:30:00"
    }



drawSchedule :: IO ()
drawSchedule = putStrLn assembleSchedule

{-
assembleSchedule :: [Timeblock] -> String
assembleSchedule blocks =
    let header = renderHeader
        body   = renderBlocks blocks
        footer = renderFooter
    in intercalate "\n" [header, body, footer]
-}


--assembleSchedule :: [Timeblock] -> String
assembleSchedule :: String
assembleSchedule =
    let header = renderHeader
        body   = renderBoilerplate
        footer = line
    in intercalate "\n" [header, body, footer]


times :: [String]
times = [show x ++ ".00" | x <- [0..23] :: [Int]]

boundaries :: [Int]
boundaries = [0, 30 .. 24 * 60]


-- HEADER RENDERING --

renderHeader :: String
renderHeader =
    "   Monday    Tuesday    Wednesday  Thursday    Friday    Saturday    Sunday"
    ++ "\n" ++ line



-- BODY RENDERING --

renderBoilerplate :: String
renderBoilerplate =
    intercalate ("\n" ++ space ++ "\n") [space ++ " " ++ toTime x | x <- boundaries]
        where
            toTime t = 
                (if  length (show (div t 60)) == 2
                then show (div t 60)
                else "0" ++ show (div t 60))
                ++ ":" ++
                (if  length (show (mod t 60)) == 2
                then show (mod t 60)
                else "0" ++ show (mod t 60))

    --intercalate ("\n" ++ line ++ "\n") [space ++ x | x <- boundaries]

{-
renderBlocks :: [Timeblock] -> String
renderBlocks blocks = ""
-}

-- render a single line
renderTimelineRow :: TimelineRow -> [Timeblock] -> String
renderTimelineRow

-- compares a time unit with a timeblock to see if the block starts at that time
startsAt :: Int -> Timeblock -> Bool


endsAt :: Int -> Timeblock -> Bool





-- FOOTER RENDERING (Likely unnecessary) --
  
renderFooter :: String
renderFooter = line


line :: String
line = "+----------+----------+----------+----------+----------+----------+----------+"
-- could also be written as:
-- line = concat (replicate 7 ("+" ++ replicate 10 '-')) ++ "+"

space :: String
space = "|          |          |          |          |          |          |          |"
-- could also be written as:
-- space = concat (replicate 7 ("|" ++ replicate 10 ' ')) ++ "|"



strToUtc :: String -> UTCTime
strToUtc s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s