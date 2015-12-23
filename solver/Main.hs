module Main where

import qualified Data.ByteString.Lazy as LBS ( readFile )
import Data.Maybe ( listToMaybe )
import GCHQ.Data.Puzzle ( readPuzzleJSON, solvePuzzle )
import System.Environment ( getArgs )

main :: IO ()
main = listToMaybe <$> getArgs >>= run where
  run Nothing = putStrLn "Invalid input file."
  run (Just filePath) = do
    fileContents <- LBS.readFile filePath
    print (solvePuzzle <$> readPuzzleJSON fileContents)
