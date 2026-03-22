-- |
-- Module      : PrettyPrint
-- Description : Pretty-printer for Pretext Task DSL expressions
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- Provides multiple rendering formats:
--
--   * 'prettyInfix'  — arrow-separated pipeline string (default)
--   * 'prettyTree'   — indented tree view (useful for debugging)
--   * 'prettyJSON'   — minimal JSON-like representation
--   * 'prettyLog'    — execution trace log (used alongside the interpreter)

module PrettyPrint
  ( -- * Rendering formats
    prettyInfix
  , prettyTree
  , prettyJSON
  , prettyLog

    -- * Data rendering helpers
  , prettyDataSample
  , prettyReport
  ) where

import Data.List (intercalate)

import Types
import DSL
import Verifier (VerificationReport (..), isClean)

-- ---------------------------------------------------------------------------
-- Infix / pipeline format
-- ---------------------------------------------------------------------------

-- | Render a 'PretextTask' as a readable pipeline string.
--
-- Examples:
--
-- @
-- rotate(90°) → permute([2,1,3]) → timeWarp(1.5)
-- (rotate(90°) ‖ timeWarp(0.8))
-- @
prettyInfix :: PretextTask -> String
prettyInfix Identity       = "identity"
prettyInfix (Rotate ang)   = "rotate(" ++ show (toAngleDegrees ang) ++ "°)"
prettyInfix (Permute p)    = "permute(" ++ show (map (+1) (getPermutation p)) ++ ")"
prettyInfix (TimeWarp wf)  = "timeWarp(" ++ showF (getWarpFactor wf) ++ ")"
prettyInfix (Seq a b)      = prettyInfix a ++ " → " ++ prettyInfix b
prettyInfix (Par a b)      = "(" ++ prettyInfix a ++ " ‖ " ++ prettyInfix b ++ ")"

-- ---------------------------------------------------------------------------
-- Tree format
-- ---------------------------------------------------------------------------

-- | Render a 'PretextTask' as an indented tree.
prettyTree :: PretextTask -> String
prettyTree = unlines . renderTree

renderTree :: PretextTask -> [String]
renderTree Identity       = ["Identity"]
renderTree (Rotate ang)   = ["Rotate " ++ show (toAngleDegrees ang) ++ "°"]
renderTree (Permute p)    = ["Permute " ++ show (map (+1) (getPermutation p))]
renderTree (TimeWarp wf)  = ["TimeWarp " ++ showF (getWarpFactor wf)]
renderTree (Seq a b)      =
  ["Seq"]
  ++ map ("  ├─ " ++) (init leftLines)
  ++ ["  ├─ " ++ last leftLines]
  ++ map ("  └─ " ++) (init rightLines)
  ++ ["  └─ " ++ last rightLines]
  where
    leftLines  = renderTree a
    rightLines = renderTree b
renderTree (Par a b)      =
  ["Par"]
  ++ map ("  ├─ " ++) (init leftLines)
  ++ ["  ├─ " ++ last leftLines]
  ++ map ("  └─ " ++) (init rightLines)
  ++ ["  └─ " ++ last rightLines]
  where
    leftLines  = renderTree a
    rightLines = renderTree b

-- ---------------------------------------------------------------------------
-- JSON-like format
-- ---------------------------------------------------------------------------

-- | Render a 'PretextTask' as a minimal JSON-like object.
prettyJSON :: PretextTask -> String
prettyJSON = go 0
  where
    indent n = replicate (n*2) ' '
    go n Identity =
      indent n ++ "{ \"type\": \"identity\" }"
    go n (Rotate ang) =
      indent n ++ "{ \"type\": \"rotate\", \"degrees\": "
               ++ show (toAngleDegrees ang) ++ " }"
    go n (Permute p) =
      indent n ++ "{ \"type\": \"permute\", \"indices\": "
               ++ show (map (+1) (getPermutation p)) ++ " }"
    go n (TimeWarp wf) =
      indent n ++ "{ \"type\": \"timeWarp\", \"factor\": "
               ++ showF (getWarpFactor wf) ++ " }"
    go n (Seq a b) =
      indent n ++ "{ \"type\": \"seq\",\n"
      ++ indent n ++ "  \"left\":\n"  ++ go (n+2) a ++ ",\n"
      ++ indent n ++ "  \"right\":\n" ++ go (n+2) b ++ "\n"
      ++ indent n ++ "}"
    go n (Par a b) =
      indent n ++ "{ \"type\": \"par\",\n"
      ++ indent n ++ "  \"left\":\n"  ++ go (n+2) a ++ ",\n"
      ++ indent n ++ "  \"right\":\n" ++ go (n+2) b ++ "\n"
      ++ indent n ++ "}"

-- ---------------------------------------------------------------------------
-- Execution log format
-- ---------------------------------------------------------------------------

-- | Build a step-by-step execution log for a task.
--   Each line describes one transformation step.
prettyLog :: PretextTask -> String
prettyLog task = unlines $ "Execution trace:" : go 1 task
  where
    go _step Identity    = ["  [no-op]"]
    go step (Rotate ang) =
      [ "  Step " ++ show step ++ ": Apply rotation " ++
        show (toAngleDegrees ang) ++ "° (clockwise)" ]
    go step (Permute p)  =
      [ "  Step " ++ show step ++ ": Apply permutation " ++
        show (map (+1) (getPermutation p)) ]
    go step (TimeWarp wf)=
      [ "  Step " ++ show step ++ ": Apply time-warp with factor " ++
        showF (getWarpFactor wf) ++
        (if getWarpFactor wf < 1
           then " (temporal compression)"
           else if getWarpFactor wf > 1
                then " (temporal stretching)"
                else " (identity warp)") ]
    go step (Seq a b)    = go step a ++ go (step + taskCount a) b
    go step (Par a b)    =
      ("  Step " ++ show step ++ ": Fork into parallel branches:") :
      map ("    [L] " ++) (go step a) ++
      map ("    [R] " ++) (go step b)

-- ---------------------------------------------------------------------------
-- Data sample rendering
-- ---------------------------------------------------------------------------

-- | Render a 'DataSample' for display in the REPL or test output.
prettyDataSample :: Show a => DataSample a -> String
prettyDataSample (SeqSample xs) =
  "Sequence [" ++ intercalate ", " (map show xs) ++ "]"
prettyDataSample (ImageSample rows) =
  "Image [\n" ++ unlines (map showRow rows) ++ "]"
  where
    showRow r = "  [" ++ intercalate ", " (map show r) ++ "]"

-- ---------------------------------------------------------------------------
-- Verification report rendering
-- ---------------------------------------------------------------------------

-- | Pretty-print a 'VerificationReport'.
prettyReport :: VerificationReport -> String
prettyReport r
  | isClean r =
      "✓ Verification passed — no errors found."
  | otherwise =
      unlines $
        (if null (staticErrors r)
           then []
           else "Static errors:" : map (("  ✗ " ++) . show) (staticErrors r))
        ++
        (if null (dynamicErrors r)
           then []
           else "Dynamic errors:" : map (("  ✗ " ++) . show) (dynamicErrors r))

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

-- | Show a Double trimming unnecessary trailing zeros.
showF :: Double -> String
showF d
  | d == fromIntegral (round d :: Int) = show (round d :: Int) ++ ".0"
  | otherwise                          = show d
