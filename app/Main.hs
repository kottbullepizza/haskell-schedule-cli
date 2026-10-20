module Main (main) where

import DrawTerminal (drawSchedule, testblock)
import Timeblock (Timeblock(..))

main :: IO ()
main = do
    drawSchedule
    print testblock
    putStrLn (timeblockTitle testblock)