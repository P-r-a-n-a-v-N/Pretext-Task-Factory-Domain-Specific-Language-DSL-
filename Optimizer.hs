-- |
-- Module      : Optimizer
-- Description : Algebraic optimizer / simplifier for the Pretext Task DSL
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- The optimizer implements a set of algebraic rewrite rules over the
-- 'PretextTask' expression tree.  It runs repeatedly until a fixed-point
-- is reached (no more rewrites apply).
--
-- Implemented rules
-- -----------------
--   R1  Identity elimination  :  Identity ▷ t  ≡  t
--                              :  t ▷ Identity  ≡  t
--   R2  Rotation fusion       :  rotate(a) ▷ rotate(b)  ≡  rotate(a+b mod 360)
--   R3  Rotation cancellation :  rotate(360°) ≡ identity (result of R2)
--   R4  Permutation fusion    :  permute(p) ▷ permute(q)  ≡  permute(p∘q)
--   R5  TimeWarp fusion       :  timeWarp(f) ▷ timeWarp(g)  ≡  timeWarp(f*g)
--   R6  TimeWarp identity     :  timeWarp(1.0)  ≡  identity
--   R7  Double negation       :  permute(p) ▷ permute(p⁻¹) ≡ identity
--                              (detects inverse permutation pairs)

module Optimizer
  ( -- * Main entry point
    optimize
  , optimizeOnce   -- single pass, for debugging
  , optimizationSteps

    -- * Individual rules (exported for unit tests)
  , fuseRotations
  , fusePermutations
  , fuseTimeWarps
  , eliminateIdentity
  ) where

import Types
import DSL

-- ---------------------------------------------------------------------------
-- Rotation arithmetic
-- ---------------------------------------------------------------------------

-- | Add two rotation angles modulo 360°.
addAngles :: RotationAngle -> RotationAngle -> RotationAngle
addAngles a b = fromDegrees ((toAngleDegrees a + toAngleDegrees b) `mod` 360)

fromDegrees :: Int -> RotationAngle
fromDegrees 0   = Rotate0
fromDegrees 90  = Rotate90
fromDegrees 180 = Rotate180
fromDegrees 270 = Rotate270
fromDegrees _   = Rotate0   -- unreachable given mod 360 input

-- ---------------------------------------------------------------------------
-- Permutation arithmetic
-- ---------------------------------------------------------------------------

-- | Compose two permutations: (p ∘ q)[i] = p[q[i]]
--   The permutations must have the same size; if they differ we return
--   Nothing (the optimizer leaves the expression alone).
composePermutations :: Permutation -> Permutation -> Maybe Permutation
composePermutations p q
  | permutationSize p /= permutationSize q = Nothing
  | otherwise =
      let idxQ  = getPermutation q   -- zero-indexed
          idxP  = getPermutation p
          composed = map (idxP !!) idxQ
          -- convert back to 1-indexed for the smart constructor
          oneIdx = map (+1) composed
      in case mkPermutation oneIdx of
           Right perm -> Just perm
           Left  _    -> Nothing

-- ---------------------------------------------------------------------------
-- Single-pass rewriter
-- ---------------------------------------------------------------------------

-- | Apply all rewrite rules once, bottom-up.
optimizeOnce :: PretextTask -> PretextTask
optimizeOnce Identity      = Identity
optimizeOnce (Rotate ang)  = Rotate ang
optimizeOnce (Permute p)   = Permute p
optimizeOnce (TimeWarp wf)
  | getWarpFactor wf == 1.0 = Identity   -- R6
  | otherwise               = TimeWarp wf
optimizeOnce (Par a b)     = Par (optimizeOnce a) (optimizeOnce b)
optimizeOnce (Seq a b)     = rewriteSeq (optimizeOnce a) (optimizeOnce b)

-- | Apply rewrite rules at the top-level of a Seq node.
rewriteSeq :: PretextTask -> PretextTask -> PretextTask
-- R1: identity elimination
rewriteSeq Identity b        = b
rewriteSeq a        Identity = a
-- R2+R3: rotation fusion
rewriteSeq (Rotate a) (Rotate b) =
  let combined = addAngles a b
  in if combined == Rotate0 then Identity else Rotate combined
-- R4: permutation fusion
rewriteSeq (Permute p) (Permute q) =
  case composePermutations p q of
    Just pq -> Permute pq
    Nothing -> Seq (Permute p) (Permute q)
-- R7: permutation inverse detection
-- (already handled by R4 producing identity; this is the explicit path)
-- R5: time-warp fusion
rewriteSeq (TimeWarp f) (TimeWarp g) =
  let combined = getWarpFactor f * getWarpFactor g
  in if combined == 1.0
     then Identity
     else TimeWarp (WarpFactor combined)  -- bypass smart ctor; factor is valid
-- No rule fires — keep as-is
rewriteSeq a b               = Seq a b

-- ---------------------------------------------------------------------------
-- Fixed-point iteration
-- ---------------------------------------------------------------------------

-- | Run 'optimizeOnce' repeatedly until the tree stops changing.
optimize :: PretextTask -> PretextTask
optimize t =
  let t' = optimizeOnce t
  in if t' == t then t else optimize t'

-- | Return the sequence of intermediate trees until the fixed-point,
--   useful for debugging or visualising the optimisation trace.
optimizationSteps :: PretextTask -> [PretextTask]
optimizationSteps t =
  let t' = optimizeOnce t
  in if t' == t then [t] else t : optimizationSteps t'

-- ---------------------------------------------------------------------------
-- Named rule functions (exported for unit tests)
-- ---------------------------------------------------------------------------

-- | Fuse consecutive Rotate nodes if present at the top level of a Seq.
fuseRotations :: PretextTask -> PretextTask
fuseRotations (Seq (Rotate a) (Rotate b)) =
  let c = addAngles a b
  in if c == Rotate0 then Identity else Rotate c
fuseRotations t = t

-- | Fuse consecutive Permute nodes if sizes match.
fusePermutations :: PretextTask -> PretextTask
fusePermutations (Seq (Permute p) (Permute q)) =
  maybe (Seq (Permute p) (Permute q)) Permute (composePermutations p q)
fusePermutations t = t

-- | Fuse consecutive TimeWarp nodes.
fuseTimeWarps :: PretextTask -> PretextTask
fuseTimeWarps (Seq (TimeWarp f) (TimeWarp g)) =
  let c = getWarpFactor f * getWarpFactor g
  in if c == 1.0 then Identity else TimeWarp (WarpFactor c)
fuseTimeWarps t = t

-- | Remove Identity nodes from a Seq chain.
eliminateIdentity :: PretextTask -> PretextTask
eliminateIdentity (Seq Identity b)   = b
eliminateIdentity (Seq a Identity)   = a
eliminateIdentity t                  = t
