-- |
-- Module      : DSL
-- Description : Embedded DSL for Self-Supervised Learning Pretext Tasks
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- This module provides the core DSL types and combinators.
-- The design follows the "final tagless" and "initial algebra" approaches:
-- we use an ADT (initial encoding) for the core representation so that
-- interpreters, optimizers, and pretty-printers can all pattern-match
-- on the same structure.
--
-- Users interact exclusively through the smart constructors and
-- combinators exported here — raw constructors are intentionally hidden
-- to preserve invariants.

module DSL
  ( -- * Core DSL type
    PretextTask (..)

    -- * Smart constructors
  , rotate
  , permute
  , timeWarp
  , identity
  , sequential   -- explicit sequential composition
  , parallel     -- run two tasks in parallel (applies both independently)

    -- * Semigroup / Monoid composition
    -- 'PretextTask' is a 'Semigroup' via sequential composition and a
    -- 'Monoid' with 'identity' as the neutral element.

    -- * Inspection helpers
  , taskDepth
  , taskCount
  , isIdentity
  , taskLabel
  ) where

import Types

-- ---------------------------------------------------------------------------
-- Core ADT
-- ---------------------------------------------------------------------------

-- | The central type of the DSL.
--
--   Every node in the expression tree is one of:
--
--   * A /leaf/ transformation ('Rotate', 'Permute', 'TimeWarp', 'Identity')
--   * A /sequential composition/ ('Seq') — apply left then right
--   * A /parallel composition/ ('Par') — apply both independently and
--     return both results (useful for multi-view SSL)
data PretextTask
  = -- | Rotate a 2D image by a validated angle.
    Rotate RotationAngle
    -- | Rearrange elements according to a validated permutation.
  | Permute Permutation
    -- | Stretch or compress a temporal sequence.
  | TimeWarp WarpFactor
    -- | The identity / no-op transformation.
  | Identity
    -- | Sequential composition: apply the left task, then the right task.
  | Seq PretextTask PretextTask
    -- | Parallel composition: apply both tasks to the same input independently.
  | Par PretextTask PretextTask
  deriving (Eq)

-- ---------------------------------------------------------------------------
-- Semigroup / Monoid — sequential composition
-- ---------------------------------------------------------------------------

-- | Sequential composition: @a <> b@ means "apply a, then apply b".
--   This makes 'PretextTask' a natural 'Semigroup'.
--
-- Optimization: Identity acts as a unit, allowing us to flatten the AST.
instance Semigroup PretextTask where
  Identity <> b = b
  a <> Identity = a
  a <> b        = Seq a b

-- | 'Identity' is the neutral element for sequential composition.
--
-- The Monoid laws are satisfied:
--   - Left identity:  @mempty <> a = a@
--   - Right identity: @a <> mempty = a@
--   - Associativity:  @(a <> b) <> c = a <> (b <> c)@ (via 'Seq' structure)
instance Monoid PretextTask where
  mempty = Identity

-- ---------------------------------------------------------------------------
-- Show instance (human-readable tree)
-- ---------------------------------------------------------------------------

instance Show PretextTask where
  show (Rotate  ang) = "rotate("  ++ show (toAngleDegrees ang) ++ "°)"
  show (Permute p  ) = "permute(" ++ show (map (+1) (getPermutation p)) ++ ")"
  show (TimeWarp w ) = "timeWarp(" ++ show (getWarpFactor w) ++ ")"
  show Identity      = "identity"
  show (Seq a b)     = show a ++ " ▷ " ++ show b
  show (Par a b)     = "(" ++ show a ++ " ‖ " ++ show b ++ ")"

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Build a rotation task.  The angle is supplied as a 'RotationAngle' so
--   only valid values can be expressed.
--
-- Usage:
-- >>> rotate Rotate90
-- rotate(90°)
rotate :: RotationAngle -> PretextTask
rotate = Rotate

-- | Build a permutation task from a /one-indexed/ list.
--   Returns 'Left' if the list is not a valid permutation.
--
-- Usage:
-- >>> permute [2,1,3]
-- Right permute([2,1,3])
permute :: [Int] -> Either DSLError PretextTask
permute xs = Permute <$> mkPermutation xs

-- | Build a time-warp task from a positive 'Double'.
--   Returns 'Left' if the factor is invalid.
--
-- Usage:
-- >>> timeWarp 1.5
-- Right timeWarp(1.5)
timeWarp :: Double -> Either DSLError PretextTask
timeWarp f = TimeWarp <$> mkWarpFactor f

-- | The identity task — leaves data unchanged.
identity :: PretextTask
identity = Identity

-- | Explicit sequential composition (same as '<>').
--
-- This is provided for users who prefer explicit naming over operators.
sequential :: PretextTask -> PretextTask -> PretextTask
sequential = (<>)

-- | Parallel composition: apply both tasks independently to the same input.
--
-- Usage:
-- >>> parallel (rotate Rotate90) (rotate Rotate180)
-- (rotate(90°) ‖ rotate(180°))
parallel :: PretextTask -> PretextTask -> PretextTask
parallel = Par

-- ---------------------------------------------------------------------------
-- Inspection helpers
-- ---------------------------------------------------------------------------

-- | The maximum depth of the task expression tree.
--
-- Identity nodes have depth 0, leaf tasks have depth 1,
-- and composite nodes add 1 to the max depth of their children.
--
-- >>> taskDepth (rotate Rotate90 <> rotate Rotate180)
-- 2
taskDepth :: PretextTask -> Int
taskDepth Identity      = 0
taskDepth (Rotate  _)   = 1
taskDepth (Permute _)   = 1
taskDepth (TimeWarp _)  = 1
taskDepth (Seq a b)     = 1 + max (taskDepth a) (taskDepth b)
taskDepth (Par a b)     = 1 + max (taskDepth a) (taskDepth b)

-- | Total number of leaf tasks (excluding Identity nodes).
--
-- This counts only non-identity transformations and does not count
-- intermediate Seq/Par nodes.
--
-- >>> taskCount (rotate Rotate90 <> rotate Rotate180)
-- 2
taskCount :: PretextTask -> Int
taskCount Identity      = 0
taskCount (Rotate  _)   = 1
taskCount (Permute _)   = 1
taskCount (TimeWarp _)  = 1
taskCount (Seq a b)     = taskCount a + taskCount b
taskCount (Par a b)     = taskCount a + taskCount b

-- | 'True' iff the task is semantically equivalent to 'Identity'.
--
-- This checks if the task:
--   * Is an explicit 'Identity' constructor
--   * Is a zero-degree rotation ('Rotate Rotate0')
--   * Is a sequence of tasks that are all identities
--   * Is a parallel composition of tasks that are all identities
--
-- >>> isIdentity (rotate Rotate0)
-- True
-- >>> isIdentity (rotate Rotate90)
-- False
isIdentity :: PretextTask -> Bool
isIdentity Identity          = True
isIdentity (Rotate Rotate0)  = True
isIdentity (Seq a b)         = isIdentity a && isIdentity b
isIdentity (Par a b)         = isIdentity a && isIdentity b
isIdentity _                 = False

-- | A short human-readable label for the top-level constructor.
--
-- This is useful for debugging and display purposes.
--
-- >>> taskLabel (rotate Rotate90)
-- "Rotate"
-- >>> taskLabel (rotate Rotate90 <> rotate Rotate180)
-- "Sequential"
taskLabel :: PretextTask -> String
taskLabel (Rotate  _) = "Rotate"
taskLabel (Permute _) = "Permute"
taskLabel (TimeWarp _)= "TimeWarp"
taskLabel Identity    = "Identity"
taskLabel (Seq _ _)   = "Sequential"
taskLabel (Par _ _)   = "Parallel"
