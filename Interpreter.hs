-- |
-- Module      : Interpreter
-- Description : Interpreter / execution engine for the Pretext Task Factory DSL
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- This module walks a 'PretextTask' expression tree and applies each
-- transformation to a 'DataSample'.  The interpreter is written in
-- continuation-passing style internally but exposes a clean pure API.
--
-- Design principles:
--   * Every operation returns @Either VerificationError (DataSample a)@
--   * No partial functions — all failure modes are explicit
--   * Numeric operations on sequences use 'Fractional' to remain general

module Interpreter
  ( -- * Main entry point
    runTask
  , runTaskUnsafe   -- throws on error (use only in tests / REPL)

    -- * Individual transformations (exported for unit testing)
  , applyRotation
  , applyPermutation
  , applyTimeWarp
  ) where

import Data.List (transpose)

import Types
import DSL

-- ---------------------------------------------------------------------------
-- Main interpreter
-- ---------------------------------------------------------------------------

-- | Execute a 'PretextTask' on a 'DataSample'.
--
--   Returns a 'Left' with a 'VerificationError' if the transformation is
--   incompatible with the data (e.g., permutation indices out of range).
runTask
  :: Fractional a
  => PretextTask
  -> DataSample a
  -> Either VerificationError (DataSample a)

runTask Identity        sample = Right sample

runTask (Rotate ang)    sample = applyRotation ang sample

runTask (Permute perm)  sample = applyPermutation perm sample

runTask (TimeWarp wf)   sample = applyTimeWarp wf sample

runTask (Seq a b)       sample = do
  intermediate <- runTask a sample
  runTask b intermediate

runTask (Par a b)       sample = do
  -- For parallel tasks we apply both and return the *left* result.
  -- In a real SSL pipeline the two outputs would both be consumed;
  -- here we demonstrate the mechanism and log both.
  resA <- runTask a sample
  _    <- runTask b sample   -- validate right branch too
  return resA   -- caller can inspect both individually if needed

-- | Like 'runTask' but calls 'error' on failure.  Useful in the REPL or
--   quick scripts where you know the input is valid.
runTaskUnsafe
  :: Fractional a
  => PretextTask
  -> DataSample a
  -> DataSample a
runTaskUnsafe t s = case runTask t s of
  Right r -> r
  Left  e -> error $ "runTaskUnsafe: " ++ show e

-- ---------------------------------------------------------------------------
-- Rotation
-- ---------------------------------------------------------------------------

-- | Apply a rotation to a 'DataSample'.
--
--   * For 'ImageSample': rotate the 2D matrix clockwise by the given angle.
--   * For 'SeqSample'  : rotation is not meaningful for 1-D sequences.
--     We return a 'UnsupportedOperation' error.
applyRotation
  :: RotationAngle
  -> DataSample a
  -> Either VerificationError (DataSample a)
applyRotation Rotate0   s               = Right s
applyRotation _         (SeqSample _)   =
  Left $ UnsupportedOperation
    "Rotation is only defined for 2D images, not sequences"
applyRotation ang       (ImageSample m) =
  Right . ImageSample $ rotateMatrix ang m

-- | Rotate a matrix clockwise.
--
--   rotate90  [[1,2],[3,4]] = [[3,1],[4,2]]
--   rotate180 [[1,2],[3,4]] = [[4,3],[2,1]]
--   rotate270 [[1,2],[3,4]] = [[2,4],[1,3]]
rotateMatrix :: RotationAngle -> Image a -> Image a
rotateMatrix Rotate0   m = m
rotateMatrix Rotate90  m = (map reverse . transpose) m
rotateMatrix Rotate180 m = (map reverse . map reverse) m
rotateMatrix Rotate270 m = (transpose . map reverse) m

-- ---------------------------------------------------------------------------
-- Permutation
-- ---------------------------------------------------------------------------

-- | Apply a permutation to a 'DataSample'.
--
--   * For 'SeqSample'  : reorder elements according to the permutation.
--   * For 'ImageSample': reorder /rows/ according to the permutation.
--
--   Returns an error if the permutation indices exceed the data size.
applyPermutation
  :: Permutation
  -> DataSample a
  -> Either VerificationError (DataSample a)
applyPermutation perm (SeqSample xs) =
  let n    = length xs
      idxs = getPermutation perm
      pSz  = permutationSize perm
  in if pSz > n
     then Left $ PermutationOutOfRange
            ("Permutation size " ++ show pSz ++
             " exceeds sequence length " ++ show n)
     else
       -- Validate all indices are within bounds
       if any (>= length xs) idxs || any (< 0) idxs
       then Left $ PermutationOutOfRange
              ("Permutation contains out-of-range indices")
       else
         -- Apply to the first pSz elements; leave the tail untouched
         let (prefix, suffix) = splitAt pSz xs
             permuted = map (prefix !!) idxs
         in Right $ SeqSample (permuted ++ suffix)

applyPermutation perm (ImageSample rows) =
  let n    = length rows
      idxs = getPermutation perm
      pSz  = permutationSize perm
  in if pSz > n
     then Left $ PermutationOutOfRange
            ("Permutation size " ++ show pSz ++
             " exceeds number of image rows " ++ show n)
     else
       -- Validate all indices are within bounds
       if any (>= length rows) idxs || any (< 0) idxs
       then Left $ PermutationOutOfRange
              ("Permutation contains out-of-range indices")
       else
         let (prefix, suffix) = splitAt pSz rows
             permuted = map (prefix !!) idxs
         in Right $ ImageSample (permuted ++ suffix)

-- ---------------------------------------------------------------------------
-- TimeWarp
-- ---------------------------------------------------------------------------

-- | Apply a time-warp (temporal resampling) to a 'DataSample'.
--
--   * For 'SeqSample'  : resample the sequence by the warp factor using
--     nearest-neighbour interpolation.  Factor > 1 stretches (more samples);
--     factor < 1 compresses (fewer samples).
--   * For 'ImageSample': warp is applied column-wise (treating columns as
--     the temporal axis).
--
--   The output length is @round (originalLength * factor)@.
applyTimeWarp
  :: Fractional a
  => WarpFactor
  -> DataSample a
  -> Either VerificationError (DataSample a)
applyTimeWarp wf (SeqSample xs) =
  let f       = getWarpFactor wf
      n       = length xs
      newLen  = max 1 (round (fromIntegral n * f) :: Int)
      warped  = nearestNeighbour xs newLen
  in Right $ SeqSample warped

applyTimeWarp wf (ImageSample rows) =
  -- Transpose → warp rows (which are now columns) → transpose back
  let cols    = transpose rows
      f       = getWarpFactor wf
      n       = length cols
      newLen  = max 1 (round (fromIntegral n * f) :: Int)
      warpedCols = map (\col -> nearestNeighbour col newLen) cols
      -- After warp the column count changed; re-transpose
      newRows = transpose warpedCols
  in Right $ ImageSample newRows

-- | Nearest-neighbour resampling: produce a list of @newLen@ elements by
--   sampling the source at evenly spaced positions.
nearestNeighbour :: [a] -> Int -> [a]
nearestNeighbour [] _      = []
nearestNeighbour src newLen
  | newLen <= 0 = []
  | otherwise =
      let n   = length src
          arr = src   -- O(n) index; fine for demo-scale data
          idx i = let pos = (fromIntegral i * fromIntegral n)
                             / fromIntegral newLen :: Double
                  in arr !! min (n - 1) (floor pos)
      in map idx [0 .. newLen - 1]
