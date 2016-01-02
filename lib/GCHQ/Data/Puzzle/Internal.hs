{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GCHQ.Data.Puzzle.Internal
  ( BitGrid
  , Index
  , Puzzle(..)
  , RowCol
  , WordGrid
  , allOr'edValidBits
  , allPossibleInts
  , allValidBits
  , allValidInts
  , checkPuzzle
  , checkRowColCount
  , checkRowColCountsEqual
  , checkSeqItemsArePositive
  , checkShadedSquares
  , checkShadedSquaresNotNull
  , checkShadingSeqsNotNull
  , checkShadingSeqsVsBounds
  , colIndicesForRow
  , gridRange
  , hintsFromWordGrid
  , ite
  , lastRowColIndex
  , preparePuzzle
  , problemAsSAT
  , rowColCount
  , rowColIndices
  , rowIndicesForCol
  , runSATSolver
  , toBools
  , toSequenceLengths
  ) where

import BitArray ( BitArray(..), toBoolList )
import Conduit
  ( Conduit, Consumer, Source, (=$=), awaitForever, enumFromToC, foldlC
  , runConduit, yield, yieldMany
  )
import Control.Applicative ( (<$>) )
import Control.Arrow ( (&&&), (***), (>>>), first, second )
import Control.Monad ( Monad, replicateM, return )
import Control.Monad.Reader ( ReaderT, asks, runReaderT )
import Control.Monad.State ( MonadState )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Array ( Array, (!), assocs, elems, listArray )
import Data.Bool ( Bool(..), (&&), not )
import Data.Either ( Either(..) )
import Data.Eq ( (==), (/=) )
import Data.Foldable ( all, and, forM_, sum )
import Data.Function ( (.), ($), flip )
import Data.Functor ( fmap )
import Data.IntMap.Strict ( IntMap, empty, insertWith )
import Data.Ix ( rangeSize )
import Data.List ( filter, genericLength, group, length, nub, null, sort, zip )
import Data.Maybe ( Maybe(..) )
import Data.Ord ( (<), (<=), (>), (>=) )
import Data.String ( String )
import Data.Tuple ( curry, fst, snd, uncurry )
import Data.Word ( Word8, Word32 )
import qualified Ersatz as E
  ( Bit, Bit1, HasSAT, Result ( Satisfied ), (===), (||), and, any, assert
  , encode, exists, minisat, solveWith
  )
import GHC.Generics ( Generic )
import Prelude ( (+), (-), (^), IO, fromIntegral, pred )
import Text.Show ( Show, show )

type RowCol = Word8
type Index = (RowCol, RowCol)
type WordGrid = Array Index Word8
type BitGrid = Array Index E.Bit1

data Puzzle = Puzzle
  { shadedSquares       :: [Index]
  , rowShadingSequences :: [[Word8]]
  , colShadingSequences :: [[Word8]]
  } deriving ( Generic, Show )

data EnvSAT = EnvSAT
  { envBitGridVars :: BitGrid
  , envPuzzle      :: Puzzle
  }

instance FromJSON Puzzle
instance ToJSON Puzzle

allOr'edValidBits
  :: (Monad m) => Consumer (RowCol, E.Bit) (ReaderT r m) (IntMap E.Bit)
allOr'edValidBits = foldlC inserter empty where
  inserter intMap (rowCol, bit) = insertWith (E.||)
                                             (fromIntegral rowCol)
                                             bit
                                             intMap

allPossibleInts :: (Monad m) => Source (ReaderT EnvSAT m) Word32
allPossibleInts = do
  p <- asks envPuzzle
  enumFromToC 0 (2 ^ rowColCount p - 1)

allValidBits :: (Monad m)
             => (RowCol -> [Index])
             -> Conduit (RowCol, [Bool]) (ReaderT EnvSAT m) (RowCol, E.Bit)
allValidBits mkIndices = awaitForever $ \(rowCol, bools) -> do
  bitGridVars <- asks envBitGridVars
  let boolsAsWords = fmap (\b -> if b then 1 else 0) bools
      encodedWords = fmap E.encode boolsAsWords
      bits = flip fmap (zip (mkIndices rowCol) encodedWords) $ \(idx, word) ->
        (bitGridVars ! idx) E.=== word
  yield (rowCol, E.and bits)

allValidInts :: (Monad m)
             => [(RowCol, [Word8])]
             -> Conduit Word32 (ReaderT r m) (RowCol, [Bool])
allValidInts rowColToShadeSeqs = awaitForever $ \i -> do
  let iBools = toBools i
      iSeqLens = toSequenceLengths iBools
      valids = filter (snd >>> (== iSeqLens)) rowColToShadeSeqs
  yieldMany $ fmap (\(rowCol, _) -> (rowCol, iBools)) valids

checkPuzzle :: Puzzle -> Maybe Puzzle
checkPuzzle p@(Puzzle ss rss css) = ite checksPassed (Just p) Nothing where
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

checkRowColCount :: [a] -> Bool
checkRowColCount = length &&& length >>> first (> 0) >>> second (<= 32)
               >>> uncurry (&&)

checkRowColCountsEqual :: [a] -> [a] -> Bool
checkRowColCountsEqual = curry (length *** length >>> uncurry (==))

checkSeqItemsArePositive :: [[Word8]] -> Bool
checkSeqItemsArePositive = and . fmap (all (> 0))

checkShadedSquares :: [[Word8]] -> [(Word8, Word8)] -> Bool
checkShadedSquares shadingSeqs = fmap (checkIdx *** checkIdx >>> uncurry (&&))
                             >>> and where
  checkIdx = (>= 0) &&& (< genericLength shadingSeqs) >>> uncurry (&&)

checkShadedSquaresNotNull :: [a] -> Bool
checkShadedSquaresNotNull = null >>> not

checkShadingSeqsNotNull :: [[a]] -> Bool
checkShadingSeqsNotNull = and . fmap (null >>> not)

checkShadingSeqsVsBounds :: [[Word8]] -> Bool
checkShadingSeqsVsBounds ss = ( fmap ( sum &&& genericLength
                                   >>> uncurry (+) >>> pred >>> (<= boundLen))
                            >>> and ) ss where
  boundLen = genericLength ss

colIndicesForRow :: Puzzle -> RowCol -> [Index]
colIndicesForRow p row = [(row, col) | col <- rowColIndices p]

gridRange :: Puzzle -> (Index, Index)
gridRange p = ((0, 0), (lastRowColIndex p, lastRowColIndex p))

hintsFromWordGrid :: WordGrid -> [(Word8, Word8)]
hintsFromWordGrid wordGrid = hints where
  hints = fmap fst . filter (second (/= 0) >>> snd) . assocs $ wordGrid

ite :: Bool -> a -> a -> a
ite b rt rf = if b then rt else rf

lastRowColIndex :: Puzzle -> Word8
lastRowColIndex p = rowColCount p - 1

preparePuzzle :: Puzzle -> Maybe Puzzle
preparePuzzle p@(Puzzle ss _ _) = Just $ p { shadedSquares = sort . nub $ ss }

problemAsSAT :: (MonadState s m, E.HasSAT s) => Puzzle -> m BitGrid
problemAsSAT puzzle@(Puzzle hintIndices inRss inCss) = do
  bitGridVars <- listArray (gridRange puzzle)
             <$> replicateM (rangeSize (gridRange puzzle)) E.exists

  forM_ (elems bitGridVars) $ \bit1 ->
    E.assert $ E.any (bit1 E.===) . fmap E.encode $ [0, 1]

  forM_ hintIndices $ \idx ->
    E.assert $ (bitGridVars ! idx) E.=== E.encode 1

  let envSAT = EnvSAT bitGridVars puzzle

  rowConstraints <- flip runReaderT envSAT $ runConduit $ allPossibleInts
    =$= allValidInts (zip (rowColIndices puzzle) inRss)
    =$= allValidBits (colIndicesForRow puzzle)
    =$= allOr'edValidBits

  colConstraints <- flip runReaderT envSAT $ runConduit $ allPossibleInts
    =$= allValidInts (zip (rowColIndices puzzle) inCss)
    =$= allValidBits (rowIndicesForCol puzzle)
    =$= allOr'edValidBits

  E.assert $ E.and rowConstraints
  E.assert $ E.and colConstraints

  return bitGridVars

rowColCount :: Puzzle -> Word8
rowColCount = rowShadingSequences >>> genericLength

rowColIndices :: Puzzle -> [Word8]
rowColIndices p = [0..(lastRowColIndex p)]

rowIndicesForCol :: Puzzle -> RowCol -> [Index]
rowIndicesForCol p col = [(row, col) | row <- rowColIndices p]

runSATSolver :: Puzzle -> IO (Either String Puzzle)
runSATSolver p = do
  (result, msolution) <- E.solveWith E.minisat (problemAsSAT p)
  ite (result /= E.Satisfied) (return . Left . show $ result) $
     case msolution of
       Nothing -> return . Left $ "No returned solution"
       Just solution -> return . Right $
         p { shadedSquares = hintsFromWordGrid solution }

toBools :: Word32 -> [Bool]
toBools = BitArray >>> toBoolList

toSequenceLengths :: [Bool] -> [Word8]
toSequenceLengths = group >>> filter and >>> fmap genericLength
