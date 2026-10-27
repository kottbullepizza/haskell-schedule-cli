module Main (main) where

import DrawTerminal (drawSchedule, testblock, testblock2)
import Timeblock (Timeblock(..))

main :: IO ()
main = do
    -- TODO: Set timezone as a storable variable in the json-file

    drawSchedule
    print testblock
    putStrLn (timeblockTitle testblock)