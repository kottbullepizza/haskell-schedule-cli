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



-- ENTRY POINTS --

drawSchedule :: IO ()
drawSchedule = do
    timeZone <- getCurrentTimeZone -- retrieve the current time zone
    putStrLn assembleSchedule

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



-- LAYOUT CONSTANTS --

cellWidth :: Int
cellWidth = 10

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



-- FOOTER RENDERING (Likely unnecessary) --
  
renderFooter :: String
renderFooter = line



-- ROW DRAWING --

renderRow :: [Bool] -> String
renderRow edges =
    concat (interleave borderStrings edgeStrings)
        where
            edgeStrings   = map fill edges
            borderStrings = map borderStr (borders edges)

borders :: [Bool] -> [Bool]
borders edges = zipWith (||) ([False] ++ edges) (edges ++ [False])

borderStr :: Bool -> String
borderStr True  = "+"
borderStr False = "|"

fill :: Bool -> String
fill True  = replicate cellWidth '-'
fill False = replicate cellWidth ' '

line :: String
line = "+----------+----------+----------+----------+----------+----------+----------+"
-- could also be written as:
-- line = concat (replicate 7 ("+" ++ replicate 10 '-')) ++ "+"

space :: String
space = "|          |          |          |          |          |          |          |"
-- could also be written as:
-- space = concat (replicate 7 ("|" ++ replicate 10 ' ')) ++ "|"



-- TIME LOGIC --

rowFlags :: TimeZone -> [Timeblock] -> [Day] -> TimeOfDay -> [Bool]
rowFlags timeZone blocks days time =
    map (\day -> cellHasEdge timeZone blocks (LocalTime day time)) days
-- could also be written as:
-- rowFlags timeZone blocks days time = [ cellHasEdge timeZone blocks (LocalTime day time) | day <- days ]

cellHasEdge :: TimeZone -> [Timeblock] -> LocalTime -> Bool
cellHasEdge timeZone blocks time = anyStartsAt || anyEndsAt
    where
        anyStartsAt = any (startsAt timeZone time) blocks
        anyEndsAt   = any (endsAt timeZone time) blocks

startsAt :: TimeZone -> LocalTime -> Timeblock -> Bool
startsAt timeZone time block =
    time == utcToLocalTime timeZone (timeblockStart block)

endsAt :: TimeZone -> LocalTime -> Timeblock -> Bool
endsAt timeZone time block =
    time == utcToLocalTime timeZone (timeblockEnd block)

strToUtc :: String -> UTCTime
strToUtc s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s



-- LIST HELPERS --

interleave :: [a] -> [a] -> [a]
interleave []     bs     = bs
interleave (a:as) bs = a : interleave bs as