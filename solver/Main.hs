{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS ( readFile, writeFile )
import Data.Maybe ( listToMaybe )
import GCHQ.Data.Puzzle ( readPuzzleJSON, solvePuzzle, writePuzzleJSON )
import System.Environment ( getArgs )

main :: IO ()
main = listToMaybe <$> getArgs >>= run where
  run Nothing = fail "ERROR: Must specify input file"
  run (Just filePath) = do
    inputPuzzleJSON  <- LBS.readFile filePath
    case readPuzzleJSON inputPuzzleJSON of
      Nothing -> fail $ "ERROR: Failed to parse JSON: " ++ filePath
      Just inputPuzzle -> do
        esolution <- solvePuzzle inputPuzzle
        case esolution of
          Left errStr -> putStrLn errStr
          Right solvedPuzzle -> LBS.writeFile ("solution.json")
                              . writePuzzleJSON
                              $ solvedPuzzle
