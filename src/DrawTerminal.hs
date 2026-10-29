module DrawTerminal (drawSchedule, testblock, testblock2) where

import Data.Time
import Data.List (intercalate)

import Timeblock (Timeblock(..), startsAt, endsAt)



testblock :: Timeblock
testblock = Timeblock
    { timeblockId          = 0
    , timeblockTitle       = "Testblock"
    , timeblockDescription = "This is a test of a time block"
    , timeblockStart       = strToUtc "2026-10-19 09:30:00"
    , timeblockEnd         = strToUtc "2026-10-19 10:30:00"
    }

testblock2 :: Timeblock
testblock2 = Timeblock
    { timeblockId          = 0
    , timeblockTitle       = "Testblock"
    , timeblockDescription = "This is a test of a time block"
    , timeblockStart       = strToUtc "2026-10-21 09:15:00"
    , timeblockEnd         = strToUtc "2026-10-21 10:45:00"
    }



-- ENTRY POINTS --

drawSchedule :: IO ()
drawSchedule = do
    timeZone <- getCurrentTimeZone
    today    <- utctDay <$> getCurrentTime
    let monday = mondayOfWeek today
    let week   = [monday .. addDays 6 monday]
    putStrLn (assembleSchedule timeZone [testblock, testblock2] week)



assembleSchedule :: TimeZone -> [Timeblock] -> [Day] -> String
assembleSchedule timeZone blocks days =
    let header = renderHeader
        body   = renderBody timeZone blocks days
        footer = renderFooter
    in intercalate "\n" [header, body, footer]



-- LAYOUT CONSTANTS --

cellWidth :: Int
cellWidth = 10

slotMinutes :: Int
slotMinutes = 30

rowMinutes :: Int
rowMinutes = 15

rowTimes :: [TimeOfDay]
rowTimes = map minutesToTime [0, rowMinutes .. 24 * 60 - rowMinutes]



-- HEADER RENDERING --

renderHeader :: String
renderHeader =
    "   Monday    Tuesday    Wednesday  Thursday    Friday    Saturday    Sunday"
    ++ "\n" ++ lineRow



-- BODY RENDERING --

renderBody :: TimeZone -> [Timeblock] -> [Day] -> String
renderBody timeZone blocks days =
    intercalate ("\n") bodyRows
    where
        bodyRows = map (renderBodyRow timeZone blocks days) rowTimes



-- PAGE SECTIONS --

renderBodyRow :: TimeZone -> [Timeblock] -> [Day] -> TimeOfDay -> String
renderBodyRow timeZone blocks days time
    | mod (todMin time) slotMinutes == 0 = renderOneRow ++ " " ++ timeLabel time
    | otherwise                          = renderOneRow
    where
        renderOneRow = renderRow (rowFlags timeZone blocks days time)

timeLabel :: TimeOfDay -> String
timeLabel time = formatTime defaultTimeLocale "%H:%M" time



-- FOOTER RENDERING (Likely unnecessary) --
  
renderFooter :: String
renderFooter = lineRow



-- ROW DRAWING --

data Cell = Blank | Edge | Inside | Text String

-- RenderWeek

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

-- remove when cell filling is fully implemented
fill :: Bool -> String
fill True  = replicate cellWidth '-'
fill False = replicate cellWidth ' '

-- TODO next: blockCell :: TimeZone -> LocalTime -> Timeblock -> Cell

cfill :: Cell -> String
cfill Edge       = replicate cellWidth '-'
cfill Blank      = replicate cellWidth ' '
cfill Inside     = replicate cellWidth ' '
cfill (Text str) = centerInWidth cellWidth str

centerInWidth :: Int -> String -> String
centerInWidth width str = replicate left ' ' ++ (take width str) ++ replicate right ' '
    where
        missing = width - length str
        left    = div missing 2
        right   = missing - left

lineRow :: String
lineRow = renderRow (replicate 7 True)

spaceRow :: String
spaceRow = renderRow (replicate 7 False)



-- TIME LOGIC --

mondayOfWeek :: Day -> Day
mondayOfWeek day = addDays (toInteger (1 - dayEnum)) day
    where
        dayEnum = fromEnum (dayOfWeek day)

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

strToUtc :: String -> UTCTime
strToUtc s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s

minutesToTime :: Int -> TimeOfDay
minutesToTime minutes = TimeOfDay hours mins 0
    where
        (hours, mins) = divMod minutes 60


-- LIST HELPERS --

interleave :: [a] -> [a] -> [a]
interleave []     bs     = bs
interleave (a:as) bs = a : interleave bs as