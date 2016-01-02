{-# LANGUAGE NoImplicitPrelude #-}

module GCHQ.Data.Puzzle
  ( Puzzle
  , readPuzzleJSON
  , writePuzzleJSON
  , solvePuzzle
  ) where

import GCHQ.Data.Puzzle.Internal
  ( Puzzle, checkPuzzle, preparePuzzle, runSATSolver )
import Control.Monad ( (>=>) )
import Data.Aeson ( decode', encode )
import qualified Data.ByteString.Lazy.Char8 as LBS ( ByteString )
import Data.Either ( Either )
import Data.Maybe ( Maybe )
import Data.String ( String )
import Prelude ( IO )

readPuzzleJSON :: LBS.ByteString -> Maybe Puzzle
readPuzzleJSON = decode' >=> checkPuzzle >=> preparePuzzle

writePuzzleJSON :: Puzzle -> LBS.ByteString
writePuzzleJSON = encode

solvePuzzle :: Puzzle -> IO (Either String Puzzle)
solvePuzzle = runSATSolver
