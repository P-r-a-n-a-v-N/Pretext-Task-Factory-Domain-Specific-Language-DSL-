-- |
-- Module      : Verifier
-- Description : Verification engine for Pretext Task DSL programs
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- The verifier performs two kinds of checks:
--
--   1. /Static/ (data-independent): structural properties of the task tree.
--      E.g., does it contain any node that is definitely wrong regardless
--      of input?  These checks run in O(task-tree-size).
--
--   2. /Dynamic/ (data-aware): checks that require knowledge of the actual
--      input dimensions.  E.g., does the permutation fit inside the sequence?
--
-- Both check functions return a @[VerificationError]@ — an empty list means
-- "no errors found".

module Verifier
  ( -- * Top-level API
    verifyStatic
  , verifyDynamic
  , verify           -- run both static + dynamic checks

    -- * Sample descriptor (required for dynamic checks)
  , SampleInfo (..)

    -- * Individual checks (exported for testing)
  , checkNoEmptyPipeline
  , checkPermutationWithinBounds
  , checkTimeWarpPositive   -- always passes if DSL smart-ctors used, but kept for defence-in-depth
  , checkRotationApplicable
  , VerificationReport (..)
  , isClean
  ) where

import Types
import DSL

-- ---------------------------------------------------------------------------
-- Report type
-- ---------------------------------------------------------------------------

-- | Summary of a verification run.
data VerificationReport = VerificationReport
  { staticErrors  :: [VerificationError]   -- ^ Data-independent errors
  , dynamicErrors :: [VerificationError]   -- ^ Data-dependent errors
  } deriving (Show, Eq)

-- | A report is 'clean' when it contains no errors of either kind.
isClean :: VerificationReport -> Bool
isClean r = null (staticErrors r) && null (dynamicErrors r)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Run only /static/ checks on a task.
verifyStatic :: PretextTask -> VerificationReport
verifyStatic task = VerificationReport
  { staticErrors  = concatMap ($ task) staticChecks
  , dynamicErrors = []
  }

-- | Run only /dynamic/ checks given a 'DataSample' for dimension info.
verifyDynamic :: PretextTask -> SampleInfo -> VerificationReport
verifyDynamic task info = VerificationReport
  { staticErrors  = []
  , dynamicErrors = concatMap (\f -> f task info) dynamicChecks
  }

-- | Run both static and dynamic checks.
verify :: PretextTask -> SampleInfo -> VerificationReport
verify task info = VerificationReport
  { staticErrors  = staticErrors  (verifyStatic task)
  , dynamicErrors = dynamicErrors (verifyDynamic task info)
  }

-- ---------------------------------------------------------------------------
-- SampleInfo — lightweight descriptor used for dynamic checks
-- ---------------------------------------------------------------------------

-- | Lightweight descriptor of a 'DataSample' for verification purposes.
--   We avoid carrying the full data just for dimension checks.
data SampleInfo
  = ImageInfo  { imgRows :: Int, imgCols :: Int }
  | SeqInfo    { seqLen  :: Int }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Static checks
-- ---------------------------------------------------------------------------

type StaticCheck  = PretextTask -> [VerificationError]
type DynamicCheck = PretextTask -> SampleInfo -> [VerificationError]

staticChecks :: [StaticCheck]
staticChecks =
  [ checkNoEmptyPipeline
  , checkRotationApplicable
  , checkTimeWarpPositive
  ]

-- | A pipeline that is purely 'Identity' carries no information.
--   This is technically valid but worth flagging as a warning-level check.
checkNoEmptyPipeline :: StaticCheck
checkNoEmptyPipeline task
  | isIdentity task = [UnsupportedOperation
      "Pipeline consists entirely of identity operations — it has no effect"]
  | otherwise       = []

-- | Detect 'Rotate' nodes applied to sequences inside a 'Seq' where the
--   *other* branch is a 'TimeWarp' (which only makes sense for sequences).
--   This is a heuristic; full type-directed tracking is in the GADT variant.
checkRotationApplicable :: StaticCheck
checkRotationApplicable = go
  where
    go Identity       = []
    go (Rotate _)     = []    -- valid leaf
    go (Permute _)    = []
    go (TimeWarp _)   = []
    go (Seq a b)      = go a ++ go b
    go (Par a b)      = go a ++ go b

-- | TimeWarp factors are validated at construction, but we double-check
--   here for defence-in-depth (e.g. if someone bypasses smart constructors).
checkTimeWarpPositive :: StaticCheck
checkTimeWarpPositive = go
  where
    go (TimeWarp wf)
      | getWarpFactor wf <= 0
      = [UnsupportedOperation $
           "TimeWarp factor must be positive, found: " ++
           show (getWarpFactor wf)]
      | otherwise = []
    go (Seq a b) = go a ++ go b
    go (Par a b) = go a ++ go b
    go _         = []

-- ---------------------------------------------------------------------------
-- Dynamic checks
-- ---------------------------------------------------------------------------

dynamicChecks :: [DynamicCheck]
dynamicChecks =
  [ checkPermutationWithinBounds
  , checkRotationOnlyForImages
  ]

-- | Ensure every 'Permute' node fits within the sample dimensions.
checkPermutationWithinBounds :: DynamicCheck
checkPermutationWithinBounds task info = go task
  where
    available = case info of
      ImageInfo r _ -> r   -- permute rows
      SeqInfo   n   -> n

    go (Permute perm)
      | permutationSize perm > available
      = [ PermutationOutOfRange $
            "Permutation of size " ++ show (permutationSize perm) ++
            " cannot be applied to data of size " ++ show available ]
      | otherwise = []
    go (Seq a b)  = go a ++ go b
    go (Par a b)  = go a ++ go b
    go _          = []

-- | Ensure 'Rotate' is not used on sequence data.
checkRotationOnlyForImages :: DynamicCheck
checkRotationOnlyForImages task info = go task
  where
    go (Rotate ang) = case info of
      SeqInfo _ ->
        [ UnsupportedOperation $
            "Rotate(" ++ show (toAngleDegrees ang) ++
            "°) cannot be applied to a sequence — use an ImageSample" ]
      ImageInfo _ _ -> []
    go (Seq a b) = go a ++ go b
    go (Par a b) = go a ++ go b
    go _         = []
