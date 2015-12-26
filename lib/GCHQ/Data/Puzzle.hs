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
import Data.List ( genericLength, find, group, groupBy, nub, sort, transpose )
import Data.Word ( Word32 )
import GHC.Generics ( Generic )

data Puzzle = Puzzle
  { shadedSquares       :: [(Word32, Word32)]
  , rowShadingSequences :: [[Word32]]
  , colShadingSequences :: [[Word32]]
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
      map (sum &&& genericLength >>> uncurry (+) >>> pred >>> (<= genericLength shadingSeqs)) shadingSeqs
    checkShadedSquares shadingSeqs = and . map (checkIndex *** checkIndex >>> uncurry (&&)) where
      checkIndex = (>= 0) &&& (< genericLength shadingSeqs) >>> uncurry (&&)

  prepare p@(Puzzle ss _ _) = Just $ p { shadedSquares = sort . nub $ ss }

writePuzzleJSON :: Puzzle -> LBS.ByteString
writePuzzleJSON = encode

solvePuzzle :: Puzzle -> Maybe Puzzle
solvePuzzle (Puzzle inSs inRss inCss) = outPuzzle where
  outPuzzle = Puzzle <$> maybeNewShadedSquares <*> Just inRss <*> Just inCss

  maybeNewShadedSquares = map (id &&& (map toBools >>> transpose >>> map toSequenceLengths >>> zipWith (==) inCss >>> and))
                      >>> find snd >>> fmap (fst >>> zip natWords >>> map (second (toBools >>> zip natWords
                      >>> filter snd >>> map fst)) >>> map (repeat *** id >>> uncurry zip) >>> concat)
                        $ allValidRowPossibilities
  allValidRowPossibilities = sequenceA allValidRowInts

  allValidRowInts = map (map snd) $ flip map allRowInts $ filter $ second (toBools >>> toSequenceLengths) >>> uncurry (==)
  allRowInts = map (first repeat >>> second (flip enumFromTo (2 ^ rowColCount - 1)) >>> uncurry zip)
             . zip inRss
             $ initRowInts

  toBools = BA.BitArray >>> BA.toBoolList
  toSequenceLengths = group >>> filter and >>> map genericLength

  initRowInts = reverse . (lastZeroes ++) . foldl' (\acc (pos, val) ->
    val : (replicate (fromIntegral (pos - genericLength acc)) 0) ++ acc) [] $ rowToInitIntPairs

  lastZeroes = flip replicate 0
             $ fromIntegral rowColCount - (fromIntegral . fst . last $ rowToInitIntPairs) - 1

  rowToInitIntPairs = map (foldr1 $ curry $ fst &&& snd *** snd >>> fst *** uncurry (.|.)) rowToSetBitPairs
  rowToSetBitPairs = map (map $ second (fromIntegral >>> shiftL (1 :: Word32))) shadedSquaresGroupedByRow
  shadedSquaresGroupedByRow = groupBy (curry $ fst *** fst >>> uncurry (==)) inSs

  rowColCount = (genericLength inRss) :: Word32
  natWords = [(0 :: Word32)..]
