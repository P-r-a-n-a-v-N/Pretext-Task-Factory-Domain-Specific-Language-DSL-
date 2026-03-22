-- |
-- Module      : Types
-- Description : Core types for the Pretext Task Factory DSL
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- This module defines all fundamental types used throughout the system.
-- We follow a design principle of making illegal states unrepresentable
-- through smart constructors and type-level constraints.

module Types
  ( -- * Data Representations
    Image
  , Sequence
  , DataSample (..)

    -- * Rotation
  , RotationAngle (..)
  , toAngleDegrees

    -- * Permutation
  , Permutation
  , mkPermutation
  , getPermutation
  , permutationSize

    -- * TimeWarp
  , WarpFactor (..)
  , mkWarpFactor
  , getWarpFactor

    -- * Error Types
  , DSLError (..)
  , VerificationError (..)

    -- * Result type alias
  , Result
  ) where

import Data.List (nub, sort)

-- ---------------------------------------------------------------------------
-- Data Representations
-- ---------------------------------------------------------------------------

-- | A 2D image represented as a row-major list of lists.
--   Invariant: all inner lists must have the same length.
type Image a = [[a]]

-- | A temporal sequence of values.
type Sequence a = [a]

-- | A tagged union of the two data kinds the DSL can operate on.
data DataSample a
  = ImageSample  (Image a)      -- ^ A 2D image (matrix)
  | SeqSample    (Sequence a)   -- ^ A 1D temporal sequence
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Rotation
-- ---------------------------------------------------------------------------

-- | Valid rotation angles expressed as an ADT.
--   Only multiples of 90° are permitted — anything else is a programming
--   error and is prevented at the type level.
data RotationAngle
  = Rotate0   -- ^ Identity rotation (no-op)
  | Rotate90  -- ^ Quarter turn clockwise
  | Rotate180 -- ^ Half turn
  | Rotate270 -- ^ Three-quarter turn clockwise (same as 90° counter-clockwise)
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert a 'RotationAngle' to its degree representation for display.
toAngleDegrees :: RotationAngle -> Int
toAngleDegrees Rotate0   = 0
toAngleDegrees Rotate90  = 90
toAngleDegrees Rotate180 = 180
toAngleDegrees Rotate270 = 270

-- ---------------------------------------------------------------------------
-- Permutation
-- ---------------------------------------------------------------------------

-- | A validated permutation stored as a zero-indexed index list.
--   Constructed only via 'mkPermutation' to guarantee correctness.
newtype Permutation = Permutation { rawIndices :: [Int] }
  deriving (Show, Eq)

-- | Smart constructor: accepts a /one-indexed/ list (user-friendly), converts
--   internally to zero-indexed and validates.
--
-- Validation rules:
--   * All indices must be in range [1 .. n]
--   * No duplicates
--   * The list must be non-empty
--
-- >>> mkPermutation [2,1,3]
-- Right (Permutation {rawIndices = [1,0,2]})
mkPermutation :: [Int] -> Either DSLError Permutation
mkPermutation [] = Left (InvalidPermutation "Permutation cannot be empty")
mkPermutation xs
  | any (<= 0) xs
  = Left $ InvalidPermutation
      ("All indices must be >= 1, got: " ++ show (filter (<= 0) xs))
  | length (nub xs) /= length xs
  = Left $ InvalidPermutation
      ("Duplicate indices found in permutation: " ++ show xs)
  | sort xs /= [1 .. length xs]
  = Left $ InvalidPermutation
      ("Indices must form a complete permutation of [1.." ++
       show (length xs) ++ "], got: " ++ show xs)
  | otherwise
  = Right $ Permutation (map (subtract 1) xs)   -- convert to 0-indexed

-- | Unwrap the zero-indexed index list.
getPermutation :: Permutation -> [Int]
getPermutation = rawIndices

-- | Return the size (number of elements) of the permutation.
permutationSize :: Permutation -> Int
permutationSize = length . rawIndices

-- ---------------------------------------------------------------------------
-- TimeWarp
-- ---------------------------------------------------------------------------

-- | A validated time-warp factor.  Must be positive and finite.
--   Values < 1 compress time; values > 1 stretch it.
newtype WarpFactor = WarpFactor { rawFactor :: Double }
  deriving (Show, Eq, Ord)

-- | Smart constructor for 'WarpFactor'.
--
-- Validation rules:
--   * Must be > 0
--   * Must be finite (no NaN / Infinity)
--
-- >>> mkWarpFactor 1.5
-- Right (WarpFactor {rawFactor = 1.5})
mkWarpFactor :: Double -> Either DSLError WarpFactor
mkWarpFactor f
  | isNaN f || isInfinite f
  = Left $ InvalidWarpFactor "WarpFactor must be a finite number"
  | f <= 0
  = Left $ InvalidWarpFactor
      ("WarpFactor must be strictly positive, got: " ++ show f)
  | otherwise
  = Right (WarpFactor f)

-- | Unwrap the raw 'Double'.
getWarpFactor :: WarpFactor -> Double
getWarpFactor = rawFactor

-- ---------------------------------------------------------------------------
-- Error Types
-- ---------------------------------------------------------------------------

-- | Errors that can be raised during DSL construction or smart-constructor
--   validation.
data DSLError
  = InvalidPermutation String   -- ^ Permutation validation failed
  | InvalidWarpFactor  String   -- ^ Warp factor validation failed
  | InvalidRotation    String   -- ^ Rotation construction failed
  | CompositionError   String   -- ^ Two tasks cannot be composed
  | EmptyPipeline               -- ^ No tasks in the pipeline
  deriving (Show, Eq)

-- | Errors produced by the /verification engine/ (run after construction).
data VerificationError
  = DimensionMismatch   String  -- ^ Transform does not match data dimensions
  | PermutationOutOfRange String -- ^ Permutation indices exceed sequence length
  | UnsupportedOperation String -- ^ Operation not applicable to data kind
  deriving (Show, Eq)

-- | Shorthand for our standard error-or-result type.
type Result a = Either DSLError a
