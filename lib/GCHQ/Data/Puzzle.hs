{-# LANGUAGE DeriveGeneric #-}

module GCHQ.Data.Puzzle
  ( Puzzle(..)
  , readPuzzleJSON
  , writePuzzleJSON
  , solvePuzzle
  ) where

import qualified BitArray as BA
import Conduit
import Control.Applicative
import Control.Arrow ( (&&&), (***), (>>>), first, second )
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson ( FromJSON, ToJSON, decode', encode )
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as LBS ( ByteString )
import Data.List ( genericLength, group, nub, sort )
import Data.Word ( Word8, Word32 )
import qualified Ersatz as E
import GHC.Generics ( Generic )

data Puzzle = Puzzle
  { shadedSquares       :: [(Word8, Word8)]
  , rowShadingSequences :: [[Word8]]
  , colShadingSequences :: [[Word8]]
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

type RowCol = Word8
type Index = (RowCol, RowCol)
type WordGrid = Array Index Word8
type BitGrid = Array Index E.Bit1

solvePuzzle :: Puzzle -> IO ()
solvePuzzle (Puzzle hintIndices inRss inCss) = run where
  run :: IO ()
  run = do
    putStrLn "Problem:"
    putStr . render $ initialWordGrid

    putStrLn "Solution:"
    (res, msol) <- E.solveWith E.minisat problem
    when (res /= E.Satisfied) $ fail (show res)
    case msol of
      Just sol -> putStr (render sol)
      _ -> fail ("sol was " ++ show msol)

  problem :: (Applicative m, MonadState s m, E.HasSAT s) => m BitGrid
  problem = do
    bitArray <- listArray gridRange
            <$> replicateM (rangeSize gridRange) E.exists

    forM_ (elems bitArray) $ \bit1 ->
      E.assert $ E.any (bit1 E.===) . map E.encode $ [0, 1]

    forM_ hintIndices $ \idx ->
      E.assert $ (bitArray ! idx) E.=== E.encode 1

    flip runReaderT bitArray $ runConduit $ allPossibleInts
      =$= allValidInts (zip rowColIndices inRss)
      =$= allValidBits colIndicesForRow
      =$= sinkList

    flip runReaderT bitArray $ runConduit $ allPossibleInts
      =$= allValidInts (zip rowColIndices inCss)
      =$= allValidBits rowIndicesForCol
      =$= sinkList

    return bitArray

  allPossibleInts :: (Monad m) => Source (ReaderT BitGrid m) Word32
  allPossibleInts = enumFromToC 0 (2 ^ rowColCount - 1)

  allValidInts :: (Monad m)
               => [(RowCol, [Word8])]
               -> Conduit Word32 (ReaderT BitGrid m) (RowCol, [Bool])
  allValidInts rowColToShadeSeqs = awaitForever $ \i -> do
    let iBools = toBools i
        iSeqLens = toSequenceLengths iBools
        valids = filter (snd >>> (== iSeqLens)) rowColToShadeSeqs
    yieldMany $ map (\(rowCol, _) -> (rowCol, iBools)) valids

  allValidBits :: (Monad m, MonadState s m, E.HasSAT s)
               => (RowCol -> [Index])
               -> Conduit (RowCol, [Bool]) (ReaderT BitGrid m) (RowCol, E.Bit)
  allValidBits mkIndices = awaitForever $ \(rowCol, bools) -> do
    bitArray <- ask
    let boolsAsWords = map (\b -> if b then 1 else 0) bools
        encodedWords = map E.encode boolsAsWords
        bits = flip map (zip (mkIndices rowCol) encodedWords) $ \(idx, word) ->
          (bitArray ! idx) E.=== word
    yield (rowCol, E.and bits)

  toBools :: Word32 -> [Bool]
  toBools = BA.BitArray >>> BA.toBoolList

  toSequenceLengths :: [Bool] -> [Word8]
  toSequenceLengths = group >>> filter and >>> map genericLength

  initialWordGrid :: WordGrid
  initialWordGrid = (array gridRange . zip gridIndices . repeat $ 0)
                 // (zip hintIndices . repeat $ 1)

  gridRange :: (Index, Index)
  gridRange = ((0, 0), (lastRowColIndex, lastRowColIndex))

  gridIndices :: [Index]
  gridIndices = [(r, c) | r <- rowColIndices, c <- rowColIndices]

  rowColIndices :: [Word8]
  rowColIndices = [0..lastRowColIndex]

  colIndicesForRow :: RowCol -> [Index]
  colIndicesForRow row = [(row, col) | col <- rowColIndices]

  rowIndicesForCol :: RowCol -> [Index]
  rowIndicesForCol col = [(row, col) | row <- rowColIndices]

  lastRowColIndex :: Word8
  lastRowColIndex = rowColCount - 1

  rowColCount :: Word8
  rowColCount = genericLength inRss

  render :: WordGrid -> String
  render sol = unlines . map renderLine $ rowColIndices where
    renderLine y = unwords . map (\x -> show (sol ! (y, x))) $ rowColIndices
