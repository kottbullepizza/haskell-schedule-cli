module Main (main) where

import DrawTerminal (drawSchedule, testblock)
import Timeblock (Timeblock(..))

main :: IO ()
main = do
    -- TODO: Set timezone as a storable variable in the json-file

    drawSchedule
    print testblock
    putStrLn (timeblockTitle testblock)