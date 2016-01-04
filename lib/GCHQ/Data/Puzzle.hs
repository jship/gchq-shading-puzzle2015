{-# LANGUAGE NoImplicitPrelude #-}

module GCHQ.Data.Puzzle
  ( Puzzle
  , mapPuzzleSquares
  , readPuzzleJSON
  , rowColCount
  , writePuzzleJSON
  , solvePuzzle
  ) where

import Control.Monad ( (>=>) )
import Data.Aeson ( decode', encode )
import qualified Data.ByteString.Lazy.Char8 as LBS ( ByteString )
import Data.Either ( Either )
import Data.Function ( (.), ($) )
import Data.Functor ( fmap )
import Data.Foldable ( foldr )
import Data.Ix ( range )
import Data.List ( insert, repeat, zip )
import Data.Maybe ( Maybe )
import Data.String ( String )
import Data.Word ( Word8 )
import GCHQ.Data.Puzzle.Internal
  ( Puzzle(..), checkPuzzle, preparePuzzle, gridRange, rowColCount
  , runSATSolver )
import Prelude ( Bool(..), IO )

mapPuzzleSquares :: (((Word8, Word8), Bool) -> a) -> Puzzle -> [a]
mapPuzzleSquares f p@(Puzzle ss _ _) = fmap f finalSquareList where
  finalSquareList = foldr insert initSquareList shadedSquareList
  initSquareList = zip (range . gridRange $ p) (repeat False)
  shadedSquareList = zip ss (repeat True)

readPuzzleJSON :: LBS.ByteString -> Maybe Puzzle
readPuzzleJSON = decode' >=> checkPuzzle >=> preparePuzzle

writePuzzleJSON :: Puzzle -> LBS.ByteString
writePuzzleJSON = encode

solvePuzzle :: Puzzle -> IO (Either String Puzzle)
solvePuzzle = runSATSolver
