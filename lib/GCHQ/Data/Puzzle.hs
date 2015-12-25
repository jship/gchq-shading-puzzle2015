{-# LANGUAGE DeriveGeneric #-}

module GCHQ.Data.Puzzle
  ( Puzzle(..)
  , readPuzzleJSON
  , writePuzzleJSON
  , solvePuzzle
  ) where

import qualified BitArray as BA ( BitArray(..), toBoolList )
import Control.Arrow ( (&&&), (***), (>>>), first, second )
import Data.Aeson ( FromJSON, ToJSON, decode', encode )
import Data.Bits ( (.|.), shiftL )
import qualified Data.ByteString.Lazy.Char8 as LBS ( ByteString )
import Data.Foldable ( foldl' )
import Data.List ( group, groupBy, nub, sort )
import Data.Tuple ( swap )
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
solvePuzzle inPuzzle@(Puzzle inSs inRss inCss) = outPuzzle where
  outPuzzle = inPuzzle { shadedSquares = newShadedSquares }
  newShadedSquares = [(0, 0)]

  allValidRowInts = map (map snd) $ flip map allRowInts $ filter $
    second (BA.BitArray >>> BA.toBoolList >>> group >>> filter (all (== True)) >>> map length) >>> uncurry (==)
  allRowInts = map (first repeat >>> second (flip enumFromTo (2 ^ rowColCount - 1)) >>> uncurry zip)
             . zip inRss
             $ initRowInts

  initRowInts = reverse . (lastZeroes ++) . foldl' (\acc (pos, val) ->
    val : (replicate (pos - length acc) 0) ++ acc) [] $ rowToInitIntPairs

  lastZeroes = flip replicate 0 $ rowColCount - (fst $ last rowToInitIntPairs) - 1

  rowToInitIntPairs = map (foldr1 $ curry $ fst &&& snd *** snd >>> fst *** uncurry (.|.)) rowToSetBitPairs
  rowToSetBitPairs = map (map $ second (shiftL (1 :: Int))) shadedSquaresGroupedByRow
  shadedSquaresGroupedByRow = groupBy (curry $ fst *** fst >>> uncurry (==)) inSs

  rowColCount = length inRss

  assocPairLeft  (a, (b, c)) = ((a, b), c)
