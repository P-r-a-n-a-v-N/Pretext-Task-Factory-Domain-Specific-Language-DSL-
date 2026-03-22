-- |
-- Module      : Generator
-- Description : Random valid Pretext Task DSL program generator
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- This module provides a deterministic pseudo-random generator for valid
-- DSL programs.  It uses a simple linear congruential generator (LCG) as
-- its randomness source so the library has zero external dependencies.
--
-- The generator guarantees that every produced 'PretextTask' passes the
-- static verifier.

module Generator
  ( -- * Seed type
    Seed

    -- * Generator monad
  , Gen
  , runGen

    -- * Random task generators
  , randomTask
  , randomLeaf
  , randomRotation
  , randomPermutation
  , randomTimeWarp
  , randomPipeline

    -- * Convenience
  , sampleTasks
  ) where

import Types
import DSL

-- ---------------------------------------------------------------------------
-- Minimal LCG pseudo-random number generator
-- ---------------------------------------------------------------------------

-- | The seed / state of the LCG.
type Seed = Int

-- | Advance the seed and return the next pseudo-random integer in [0, m).
lcgNext :: Seed -> (Int, Seed)
lcgNext s = (v, s')
  where
    -- Knuth's constants for a 32-bit LCG
    s' = (1664525 * s + 1013904223) `mod` (2^(31 :: Int))
    v  = abs s'

-- | A tiny state-passing "monad" for random generation.
--   We roll our own rather than import System.Random to stay dependency-free.
newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Functor Gen where
  fmap f (Gen g) = Gen $ \s -> let (a, s') = g s in (f a, s')

instance Applicative Gen where
  pure x  = Gen $ \s -> (x, s)
  Gen f <*> Gen x = Gen $ \s ->
    let (fn, s')  = f s
        (arg, s'') = x s'
    in (fn arg, s'')

instance Monad Gen where
  return  = pure
  Gen x >>= f = Gen $ \s ->
    let (a, s')  = x s
        Gen g    = f a
    in g s'

-- | Produce a random Int in [lo, hi].
randInt :: Int -> Int -> Gen Int
randInt lo hi = Gen $ \s ->
  let (v, s') = lcgNext s
      r       = lo + v `mod` (hi - lo + 1)
  in (r, s')

-- | Produce a random Double in [lo, hi).
randDouble :: Double -> Double -> Gen Double
randDouble lo hi = Gen $ \s ->
  let (v, s') = lcgNext s
      r       = lo + (fromIntegral v / fromIntegral (2^(31 :: Int) :: Int)) * (hi - lo)
  in (r, s')

-- | Pick a random element from a non-empty list.
randElem :: [a] -> Gen a
randElem xs = do
  i <- randInt 0 (length xs - 1)
  return (xs !! i)

-- ---------------------------------------------------------------------------
-- Random permutation generator (Fisher-Yates)
-- ---------------------------------------------------------------------------

-- | Generate a random permutation of [1..n].
randPermutation :: Int -> Gen [Int]
randPermutation n = go [1..n] []
  where
    go []  acc = return (reverse acc)
    go xs  acc = do
      i <- randInt 0 (length xs - 1)
      let x  = xs !! i
          xs' = take i xs ++ drop (i+1) xs
      go xs' (x:acc)

-- ---------------------------------------------------------------------------
-- Random task generators
-- ---------------------------------------------------------------------------

-- | Generate a random rotation task.
randomRotation :: Gen PretextTask
randomRotation = do
  ang <- randElem [Rotate90, Rotate180, Rotate270]
  return (rotate ang)

-- | Generate a random permutation task of size between 2 and maxSz.
randomPermutation :: Int -> Gen (Either DSLError PretextTask)
randomPermutation maxSz = do
  sz <- randInt 2 (max 2 maxSz)
  indices <- randPermutation sz
  return (permute indices)

-- | Generate a random time-warp task with factor in [0.5, 2.0].
randomTimeWarp :: Gen (Either DSLError PretextTask)
randomTimeWarp = do
  f <- randDouble 0.5 2.0
  return (timeWarp f)

-- | Generate a random leaf task (rotation, permutation, or time-warp).
--   Invalid constructions are retried automatically (extremely rare in practice).
randomLeaf :: Int -> Gen PretextTask
randomLeaf maxPermSize = do
  choice <- randInt 0 2
  case choice of
    0 -> randomRotation
    1 -> do
      r <- randomPermutation maxPermSize
      case r of
        Right t -> return t
        Left  _ -> randomRotation   -- fallback
    _ -> do
      r <- randomTimeWarp
      case r of
        Right t -> return t
        Left  _ -> randomRotation   -- fallback (never fires for valid doubles)

-- | Generate a random 'PretextTask' tree of depth up to @maxDepth@.
randomTask :: Int -> Int -> Gen PretextTask
randomTask 0 maxPerm = randomLeaf maxPerm
randomTask maxDepth maxPerm = do
  choice <- randInt 0 3
  case choice of
    0 -> randomLeaf maxPerm
    1 -> do
      a <- randomTask (maxDepth-1) maxPerm
      b <- randomTask (maxDepth-1) maxPerm
      return (sequential a b)
    2 -> do
      a <- randomTask (maxDepth-1) maxPerm
      b <- randomTask (maxDepth-1) maxPerm
      return (parallel a b)
    _ -> randomLeaf maxPerm

-- | Generate a random sequential pipeline of @len@ leaf tasks.
randomPipeline :: Int -> Int -> Gen PretextTask
randomPipeline 0   _       = return identity
randomPipeline len maxPerm = do
  leaf <- randomLeaf maxPerm
  rest <- randomPipeline (len-1) maxPerm
  return (sequential leaf rest)

-- ---------------------------------------------------------------------------
-- Convenience
-- ---------------------------------------------------------------------------

-- | Generate @n@ independent random tasks using successive seeds.
--
-- >>> sampleTasks 42 5
-- [...]
sampleTasks :: Seed -> Int -> [PretextTask]
sampleTasks seed n = fst $ go seed n []
  where
    go s 0 acc = (reverse acc, s)
    go s k acc =
      let (task, s') = runGen (randomTask 3 4) s
      in go s' (k-1) (task:acc)
