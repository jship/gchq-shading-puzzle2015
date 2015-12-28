{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS ( readFile, writeFile )
import Data.Maybe ( listToMaybe )
import GCHQ.Data.Puzzle ( readPuzzleJSON, solvePuzzle, solvePuzzle2, writePuzzleJSON )
import System.Environment ( getArgs )

main :: IO ()
main = listToMaybe <$> getArgs >>= run where
  run Nothing = putStrLn "ERROR: Must specify input file"
  run (Just filePath) = do
    inputPuzzleJSON  <- LBS.readFile filePath
    case readPuzzleJSON inputPuzzleJSON of
      Nothing -> putStrLn $ "ERROR: Failed to parse JSON: " ++ filePath
      --Just inputPuzzle -> case solvePuzzle inputPuzzle of
      --  Nothing -> putStrLn $ "ERROR: Failed to solve puzzle: " ++ filePath
      --  Just solvedPuzzle -> LBS.writeFile ("solution.json")
      --                     . writePuzzleJSON
      --                     $ solvedPuzzle
      Just inputPuzzle -> solvePuzzle2 inputPuzzle
