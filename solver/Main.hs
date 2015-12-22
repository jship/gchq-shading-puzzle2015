module Main where

import qualified Data.ByteString.Lazy as LBS ( readFile )
import GCHQ.Data.Puzzle ( readPuzzleJSON, solvePuzzle )
import System.Environment ( getArgs )

main :: IO ()
main = head <$> getArgs >>= LBS.readFile >>= print . readPuzzleJSON
