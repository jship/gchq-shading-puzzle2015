{-# LANGUAGE DeriveGeneric #-}

module GCHQ.Data.Puzzle
  ( Puzzle(..)
  , readPuzzleJSON
  , writePuzzleJSON
  , solvePuzzle
  ) where

import Control.Arrow ( (&&&), (***), (>>>), first, second )
import Data.Aeson ( FromJSON, ToJSON, decode', encode )
import qualified Data.ByteString.Lazy.Char8 as LBS ( ByteString )
import Data.List ( genericLength, nub, sort )
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

solvePuzzle :: Puzzle -> IO ()
solvePuzzle (Puzzle inSs inRss inCss) = return ()
