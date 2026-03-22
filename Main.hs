-- |
-- Module      : Main
-- Description : Demonstration driver for the Pretext Task Factory
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- Run with:
--
-- @
-- cabal run pretext-task-factory
-- @

module Main (main) where

import Types
import DSL
import Interpreter
import Verifier
import Optimizer
import PrettyPrint
import Generator

import Data.Either (fromRight)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

section :: String -> IO ()
section title = do
  putStrLn ""
  putStrLn $ replicate 65 '='
  putStrLn $ "  " ++ title
  putStrLn $ replicate 65 '='

subsection :: String -> IO ()
subsection t = putStrLn $ "\n  -- " ++ t

printResult :: Show a => Either VerificationError (DataSample a) -> IO ()
printResult (Left  e) = putStrLn $ "  ERROR: " ++ show e
printResult (Right s) = putStrLn $ "  " ++ prettyDataSample s

-- ---------------------------------------------------------------------------
-- Sample data
-- ---------------------------------------------------------------------------

-- | A small 4x4 grayscale image (pixel values 0.0 to 1.0).
sampleImage :: DataSample Double
sampleImage = ImageSample
  [ [0.1, 0.2, 0.3, 0.4]
  , [0.5, 0.6, 0.7, 0.8]
  , [0.9, 1.0, 0.0, 0.1]
  , [0.2, 0.3, 0.4, 0.5]
  ]

-- | A simple temporal sequence.
sampleSeq :: DataSample Double
sampleSeq = SeqSample [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

-- ---------------------------------------------------------------------------
-- Demo 1 -- Basic tasks on image data
-- ---------------------------------------------------------------------------

demo1 :: IO ()
demo1 = do
  section "Demo 1 -- Basic Transformations on an Image"

  subsection "Original image:"
  putStrLn $ "  " ++ prettyDataSample sampleImage

  -- Rotation 90
  subsection "rotate(90 deg):"
  let rotTask = rotate Rotate90
  putStrLn $ "  DSL expression   : " ++ prettyInfix rotTask
  putStrLn $ "  Verify (static)  : " ++ prettyReport (verifyStatic rotTask)
  putStrLn $ "  Verify (dynamic) : "
          ++ prettyReport (verify rotTask (ImageInfo 4 4))
  printResult (runTask rotTask sampleImage)

  -- Rotation 180
  subsection "rotate(180 deg):"
  let rot180 = rotate Rotate180
  printResult (runTask rot180 sampleImage)

  -- Rotation fusion demo
  subsection "rotate(90) >> rotate(270) [cancels to identity]:"
  let fused = rotate Rotate90 <> rotate Rotate270
  putStrLn $ "  Before optimise: " ++ prettyInfix fused
  putStrLn $ "  After  optimise: " ++ prettyInfix (optimize fused)

-- ---------------------------------------------------------------------------
-- Demo 2 -- Permutation on sequence data
-- ---------------------------------------------------------------------------

demo2 :: IO ()
demo2 = do
  section "Demo 2 -- Permutation on Sequence Data"

  subsection "Original sequence:"
  putStrLn $ "  " ++ prettyDataSample sampleSeq

  subsection "permute([2,1,3,4,5,6,7,8]) -- swap first two elements:"
  case permute [2,1,3,4,5,6,7,8] of
    Left  err   -> putStrLn $ "  Build error: " ++ show err
    Right pTask -> do
      let report = verify pTask (SeqInfo 8)
      putStrLn $ "  DSL expression : " ++ prettyInfix pTask
      putStrLn $ "  Verify         : " ++ prettyReport report
      printResult (runTask pTask sampleSeq)

  subsection "permute([3,1,2]) -- applied to first 3 elements:"
  case permute [3,1,2] of
    Left  err   -> putStrLn $ "  Build error: " ++ show err
    Right pTask -> printResult (runTask pTask sampleSeq)

  subsection "permute([5,4,3,2,1]) -- reverse first 5 elements:"
  case permute [5,4,3,2,1] of
    Left  err   -> putStrLn $ "  Build error: " ++ show err
    Right pTask -> printResult (runTask pTask sampleSeq)

  subsection "Invalid permutation -- duplicate indices [1,1,3]:"
  case permute [1,1,3] of
    Left  err -> putStrLn $ "  Correctly rejected: " ++ show err
    Right _   -> putStrLn $ "  BUG: Should have failed!"

-- ---------------------------------------------------------------------------
-- Demo 3 -- TimeWarp on sequence data
-- ---------------------------------------------------------------------------

demo3 :: IO ()
demo3 = do
  section "Demo 3 -- TimeWarp (Temporal Resampling)"

  subsection "Original sequence [1..8]:"
  putStrLn $ "  " ++ prettyDataSample sampleSeq

  subsection "timeWarp(2.0) -- stretch to 16 samples:"
  case timeWarp 2.0 of
    Left  err   -> putStrLn $ "  Build error: " ++ show err
    Right wTask -> do
      putStrLn $ "  " ++ prettyInfix wTask
      printResult (runTask wTask sampleSeq)

  subsection "timeWarp(0.5) -- compress to 4 samples:"
  case timeWarp 0.5 of
    Left  err   -> putStrLn $ "  Build error: " ++ show err
    Right wTask -> printResult (runTask wTask sampleSeq)

  subsection "timeWarp(1.0) -- identity warp:"
  case timeWarp 1.0 of
    Left  err   -> putStrLn $ "  Build error: " ++ show err
    Right wTask -> do
      putStrLn $ "  Before optimise: " ++ prettyInfix wTask
      putStrLn $ "  After  optimise: " ++ prettyInfix (optimize wTask)

  subsection "Invalid timeWarp(-0.5):"
  case timeWarp (-0.5) of
    Left  err -> putStrLn $ "  Correctly rejected: " ++ show err
    Right _   -> putStrLn $ "  BUG: Should have failed!"

-- ---------------------------------------------------------------------------
-- Demo 4 -- Pipeline composition
-- ---------------------------------------------------------------------------

demo4 :: IO ()
demo4 = do
  section "Demo 4 -- Pipeline Composition"

  subsection "Building: rotate(90) >> permute([2,1,3,4]) >> timeWarp(1.5)"
  case buildPipeline of
    Left  err      -> putStrLn $ "  Build error: " ++ show err
    Right pipeline -> do
      putStrLn $ "  DSL infix  : " ++ prettyInfix pipeline
      putStrLn $ "  Task count : " ++ show (taskCount pipeline)
      putStrLn $ "  Tree depth : " ++ show (taskDepth pipeline)
      putStrLn ""
      putStrLn   "  Execution log:"
      putStr   $ unlines (map ("  " ++) (lines (prettyLog pipeline)))
      subsection "  Execution on image:"
      printResult (runTask pipeline sampleImage)

  subsection "Monoid-style accumulation:"
  let tasks = mconcat [ rotate Rotate90
                       , rotate Rotate180
                       , fromRight identity (timeWarp 0.8)
                       ]
  putStrLn $ "  " ++ prettyInfix tasks
  putStrLn $ "  Optimised: " ++ prettyInfix (optimize tasks)

  where
    buildPipeline :: Either DSLError PretextTask
    buildPipeline = do
      pTask <- permute [2,1,3,4]
      wTask <- timeWarp 1.5
      return $ rotate Rotate90 <> pTask <> wTask

-- ---------------------------------------------------------------------------
-- Demo 5 -- Optimizer
-- ---------------------------------------------------------------------------

demo5 :: IO ()
demo5 = do
  section "Demo 5 -- Optimizer / Algebraic Simplification"

  subsection "Rule R2+R3: rotate(90) >> rotate(270) -> identity"
  let r1 = rotate Rotate90 <> rotate Rotate270
  putStrLn $ "  Before: " ++ prettyInfix r1
  putStrLn $ "  After : " ++ prettyInfix (optimize r1)

  subsection "Rule R2: rotate(90) >> rotate(90) -> rotate(180)"
  let r2 = rotate Rotate90 <> rotate Rotate90
  putStrLn $ "  Before: " ++ prettyInfix r2
  putStrLn $ "  After : " ++ prettyInfix (optimize r2)

  subsection "Rule R5: timeWarp(2.0) >> timeWarp(0.5) -> identity"
  case (,) <$> timeWarp 2.0 <*> timeWarp 0.5 of
    Left  err    -> putStrLn $ "  Build error: " ++ show err
    Right (a, b) -> do
      let chain = a <> b
      putStrLn $ "  Before: " ++ prettyInfix chain
      putStrLn $ "  After : " ++ prettyInfix (optimize chain)

  subsection "Rule R4: permute([2,1,3]) >> permute([1,3,2]) (composition)"
  case (,) <$> permute [2,1,3] <*> permute [1,3,2] of
    Left  err    -> putStrLn $ "  Build error: " ++ show err
    Right (a, b) -> do
      let chain = a <> b
      putStrLn $ "  Before: " ++ prettyInfix chain
      putStrLn $ "  After : " ++ prettyInfix (optimize chain)

  subsection "Optimisation trace (step-by-step):"
  let complexTask =
        rotate Rotate90 <> rotate Rotate270 <>
        fromRight identity (timeWarp 1.0) <>
        rotate Rotate180
  putStrLn $ "  Input: " ++ prettyInfix complexTask
  mapM_ (\(i, t) -> putStrLn $ "  Step " ++ show (i :: Int) ++ ": " ++ prettyInfix t)
        (zip [0..] (optimizationSteps complexTask))

-- ---------------------------------------------------------------------------
-- Demo 6 -- Verification engine
-- ---------------------------------------------------------------------------

demo6 :: IO ()
demo6 = do
  section "Demo 6 -- Verification Engine"

  subsection "Static: pipeline of only identity tasks"
  let idTask = identity <> identity
  putStr $ prettyReport (verifyStatic idTask)

  subsection "Dynamic: permutation too large for sequence"
  case permute [4,3,2,1] of
    Left  err -> putStrLn $ "  Build error: " ++ show err
    Right p4  -> do
      let info = SeqInfo 3   -- sequence of only 3 elements
          rep  = verify p4 info
      putStr $ prettyReport rep

  subsection "Dynamic: rotation on sequence (invalid)"
  let rotTask = rotate Rotate90
      rep     = verify rotTask (SeqInfo 8)
  putStr $ prettyReport rep

  subsection "Valid pipeline passes all checks"
  case permute [2,1,3,4] of
    Left  err -> putStrLn $ "  Build error: " ++ show err
    Right p   -> do
      let pipeline = rotate Rotate90 <> p
          rep      = verify pipeline (ImageInfo 4 4)
      putStr $ prettyReport rep

-- ---------------------------------------------------------------------------
-- Demo 7 -- Pretty printing formats
-- ---------------------------------------------------------------------------

demo7 :: IO ()
demo7 = do
  section "Demo 7 -- Pretty Printing Formats"

  case buildTask of
    Left err   -> putStrLn $ "  Build error: " ++ show err
    Right task -> do
      subsection "Infix format:"
      putStrLn $ "  " ++ prettyInfix task

      subsection "Tree format:"
      putStr . unlines . map ("  " ++) . lines $ prettyTree task

      subsection "JSON format:"
      putStr . unlines . map ("  " ++) . lines $ prettyJSON task

      subsection "Log format:"
      putStr . unlines . map ("  " ++) . lines $ prettyLog task

  where
    buildTask :: Either DSLError PretextTask
    buildTask = do
      p <- permute [2,1,3]
      w <- timeWarp 1.5
      return $ rotate Rotate90 <> p <> w

-- ---------------------------------------------------------------------------
-- Demo 8 -- Random task generator
-- ---------------------------------------------------------------------------

demo8 :: IO ()
demo8 = do
  section "Demo 8 -- Random Task Generator"

  let tasks = sampleTasks 12345 6
  mapM_ (\(i, t) -> do
    putStrLn $ "  [" ++ show (i :: Int) ++ "] " ++ prettyInfix t
    putStrLn $ "      Optimised : " ++ prettyInfix (optimize t)
    putStrLn $ "      Leaf count: " ++ show (taskCount t)
    putStrLn ""
    ) (zip [1..] tasks)

-- ---------------------------------------------------------------------------
-- Demo 9 -- Parallel tasks
-- ---------------------------------------------------------------------------

demo9 :: IO ()
demo9 = do
  section "Demo 9 -- Parallel Composition (Multi-View SSL)"

  subsection "Two views: rotate(90) || rotate(270)"
  let view1     = rotate Rotate90
      view2     = rotate Rotate270
      bothViews = parallel view1 view2

  putStrLn $ "  Expression: " ++ prettyInfix bothViews
  subsection "  Apply to image -- left branch result:"
  printResult (runTask bothViews sampleImage)

  subsection "  Right branch result (view2 only):"
  printResult (runTask view2 sampleImage)

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  putStrLn "================================================================="
  putStrLn "       Pretext Task Factory -- DSL Demonstration"
  putStrLn "   A Type-Safe DSL for Self-Supervised Learning Pretext Tasks"
  putStrLn "================================================================="

  demo1
  demo2
  demo3
  demo4
  demo5
  demo6
  demo7
  demo8
  demo9

  putStrLn ""
  putStrLn $ replicate 65 '='
  putStrLn "  All demonstrations complete."
  putStrLn $ replicate 65 '='
  putStrLn ""
