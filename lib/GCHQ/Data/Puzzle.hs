{-# LANGUAGE DeriveGeneric #-}

module GCHQ.Data.Puzzle
  ( Puzzle
  , readPuzzleJSON
  , writePuzzleJSON
  , solvePuzzle
  ) where

import Control.Arrow ( (&&&), (***), (>>>), first, second )
import Control.Monad ( replicateM )
import Data.Aeson ( FromJSON, ToJSON, decode', encode )
import Data.Bits ( (.|.), shiftL )
import qualified Data.ByteString.Lazy as LBS ( ByteString )
import Data.List ( groupBy, nub, sort )
import GHC.Generics ( Generic )

data Puzzle = Puzzle
  { shadedSquares       :: [(Int, Int)]
  , rowShadingSequences :: [[Int]]
  , colShadingSequences :: [[Int]]
  } deriving ( Generic, Show )

instance FromJSON Puzzle
instance ToJSON Puzzle

readPuzzleJSON :: LBS.ByteString -> Maybe Puzzle
readPuzzleJSON bs = decode' bs >>= check >>= prepare where
  check p@(Puzzle ss rss css) = if checksPassed then Just p else Nothing where
    checksPassed = and
      [ checkShadedSquaresNotNull ss
      , checkShadingSeqsNotNull rss
      , checkShadingSeqsNotNull css
      , checkRowColCount rss
      , checkRowColCount css
      , checkRowColCountsEqual rss css
      , checkSeqItemsArePositive rss
      , checkSeqItemsArePositive css
      , checkShadingSeqsVsBounds rss
      , checkShadingSeqsVsBounds css
      , checkShadedSquares rss ss
      ]

    checkShadedSquaresNotNull = null >>> not
    checkShadingSeqsNotNull = and . map (null >>> not)
    checkRowColCount = length &&& length >>> first (> 0) >>> second (<= 32) >>> uncurry (&&)
    checkRowColCountsEqual = curry (length *** length >>> uncurry (==))
    checkSeqItemsArePositive = and . map (all (> 0))
    checkShadingSeqsVsBounds shadingSeqs = and $
      map (sum &&& length >>> uncurry (+) >>> pred >>> (<= length shadingSeqs)) shadingSeqs
    checkShadedSquares shadingSeqs = and . map (checkIndex *** checkIndex >>> uncurry (&&)) where
      checkIndex = (>= 0) &&& (< length shadingSeqs) >>> uncurry (&&)

  prepare p@(Puzzle ss _ _) = Just $ p { shadedSquares = sort . nub $ ss }

writePuzzleJSON :: Puzzle -> LBS.ByteString
writePuzzleJSON = encode

solvePuzzle :: Puzzle -> Puzzle
solvePuzzle inPuzzle = outPuzzle where
  outPuzzle = inPuzzle
  inShadedSquares = shadedSquares inPuzzle
  inRowShadingSequences = rowShadingSequences inPuzzle
  inColShadingSequences = colShadingSequences inPuzzle
  rowColCount = length inRowShadingSequences

  -- [(0, 5), (1, 4)]
  rowToInitIntPairs = map (foldr1 $ curry $ fst &&& snd *** snd >>> fst *** uncurry (.|.)) rowToSetBitPairs

  -- [[(0, 1), (0, 4)], [(1, 2)], ...]
  rowToSetBitPairs = map (map $ second (shiftL (1 :: Int))) shadedSquaresGroupedByRow

  -- [[(0, 0), (0, 2)], [(1, 1)], ...]
  shadedSquaresGroupedByRow = groupBy (curry $ fst *** fst >>> uncurry (==)) inShadedSquares
