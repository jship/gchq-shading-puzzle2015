{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS ( readFile )
import Data.Maybe ( listToMaybe )
import Data.Word ( Word8 )
import GCHQ.Data.Puzzle
  ( Puzzle, mapPuzzleSquares, readPuzzleJSON, rowColCount )
import Graphics.Gloss
import System.Environment ( getArgs )

windowConfig :: Display
windowConfig = InWindow "Puzzle" (winWidth, winWidth) (10, 10)

winWidth :: Int
winWidth = 400

backgroundColor :: Color
backgroundColor = violet

gridPic :: Puzzle -> Picture
gridPic puzzle = rotate 90.0
               . translate gridTran gridTran
               . pictures
               $ squarePics where
  squarePics = mapPuzzleSquares toPic puzzle
  toPic :: (((Word8, Word8), Bool) -> Picture)
  toPic ((row, col), isShaded) = pic where
    pic = translate tranX tranY
        . color rectColor
        $ rectangleSolid squareEdgeLength squareEdgeLength
    tranX, tranY :: Float
    tranX = fromIntegral row * squareEdgeLength
    tranY = fromIntegral col * squareEdgeLength
    rectColor = if isShaded then black else white
  squareEdgeLength = fromIntegral winWidth / fromIntegral squaresPerSide
  squaresPerSide = rowColCount puzzle
  gridTran = (-0.5 * fromIntegral winWidth) + (0.5 * squareEdgeLength)

main :: IO ()
main = listToMaybe <$> getArgs >>= run where
  run Nothing = fail "ERROR: Must specify input file"
  run (Just filePath) = do
    inputPuzzleJSON  <- LBS.readFile filePath
    case readPuzzleJSON inputPuzzleJSON of
      Nothing -> fail $ "ERROR: Failed to parse JSON: " ++ filePath
      Just inputPuzzle -> display windowConfig backgroundColor
                        . gridPic
                        $ inputPuzzle
