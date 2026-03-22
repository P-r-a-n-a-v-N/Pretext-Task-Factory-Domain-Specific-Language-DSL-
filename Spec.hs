-- |
-- Module      : Main (Spec)
-- Description : Test suite for the Pretext Task Factory
-- Copyright   : (c) 2025 Pretext Task Factory
-- License     : MIT
--
-- Pure functional tests -- no external testing framework required.
-- Run with:   cabal test --test-show-details=always

module Main (main) where

import Types
import DSL
import Interpreter
import Verifier
import Optimizer
import PrettyPrint
import Generator

import Data.Either (isLeft, isRight, fromRight)

-- ---------------------------------------------------------------------------
-- Tiny test framework (no dependencies)
-- ---------------------------------------------------------------------------

data TestResult = Pass String | Fail String String

isPassed :: TestResult -> Bool
isPassed (Pass _) = True
isPassed _        = False

assert :: String -> Bool -> TestResult
assert name True  = Pass name
assert name False = Fail name "Assertion failed (expected True)"

assertEqual :: (Show a, Eq a) => String -> a -> a -> TestResult
assertEqual name expected actual
  | expected == actual = Pass name
  | otherwise          = Fail name $
      "Expected: " ++ show expected ++ "\n  Got    : " ++ show actual

runTests :: [(String, [TestResult])] -> IO ()
runTests groups = do
  let allResults = concatMap snd groups
      passed     = length (filter isPassed allResults)
      total      = length allResults
  mapM_ printGroup groups
  putStrLn $ replicate 60 '-'
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " passed"
  if passed == total
    then putStrLn "All tests passed!"
    else mapM_ printFailed allResults
  where
    printGroup (name, results) = do
      putStrLn $ "\n>> " ++ name
      mapM_ printOne results
    printOne (Pass n)   = putStrLn $ "  [OK]   " ++ n
    printOne (Fail n m) = putStrLn $ "  [FAIL] " ++ n ++ " | " ++ m
    printFailed (Fail n m) =
      putStrLn $ "\nFAILED: " ++ n ++ "\n  " ++ m
    printFailed (Pass _)   = return ()

-- ---------------------------------------------------------------------------
-- Helper: substring check
-- ---------------------------------------------------------------------------

isSubStr :: String -> String -> Bool
isSubStr needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf p s = take (length p) s == p
    tails []     = [[]]
    tails xs@(_:rest) = xs : tails rest

-- ---------------------------------------------------------------------------
-- Test data
-- ---------------------------------------------------------------------------

img2x2 :: DataSample Double
img2x2 = ImageSample [[1,2],[3,4]]

img4x4 :: DataSample Double
img4x4 = ImageSample
  [ [1,2,3,4]
  , [5,6,7,8]
  , [9,10,11,12]
  , [13,14,15,16]
  ]

seq8 :: DataSample Double
seq8 = SeqSample [1,2,3,4,5,6,7,8]

-- ---------------------------------------------------------------------------
-- Types tests
-- ---------------------------------------------------------------------------

typesTests :: [TestResult]
typesTests =
  [ assertEqual "mkPermutation [2,1,3] gives zero-indexed [1,0,2]"
      (Right [1,0,2])
      (fmap getPermutation (mkPermutation [2,1,3]))

  , assert "mkPermutation rejects empty"
      (isLeft (mkPermutation ([] :: [Int])))

  , assert "mkPermutation rejects duplicates [1,1,3]"
      (isLeft (mkPermutation [1,1,3]))

  , assert "mkPermutation rejects zero index [0,1,2]"
      (isLeft (mkPermutation [0,1,2]))

  , assert "mkPermutation rejects non-surjective [1,2,4]"
      (isLeft (mkPermutation [1,2,4]))

  , assert "mkWarpFactor accepts 1.5"
      (isRight (mkWarpFactor 1.5))

  , assert "mkWarpFactor rejects 0.0"
      (isLeft (mkWarpFactor 0.0))

  , assert "mkWarpFactor rejects negative"
      (isLeft (mkWarpFactor (-2.0)))

  , assert "mkWarpFactor rejects NaN"
      (isLeft (mkWarpFactor (0/0)))

  , assert "mkWarpFactor rejects Infinity"
      (isLeft (mkWarpFactor (1/0)))

  , assertEqual "toAngleDegrees Rotate90"  90  (toAngleDegrees Rotate90)
  , assertEqual "toAngleDegrees Rotate0"   0   (toAngleDegrees Rotate0)
  , assertEqual "toAngleDegrees Rotate180" 180 (toAngleDegrees Rotate180)
  , assertEqual "toAngleDegrees Rotate270" 270 (toAngleDegrees Rotate270)
  ]

-- ---------------------------------------------------------------------------
-- DSL tests
-- ---------------------------------------------------------------------------

dslTests :: [TestResult]
dslTests =
  [ assertEqual "rotate smart ctor"
      (Rotate Rotate90) (rotate Rotate90)

  , assert "permute valid [2,1,3]"
      (isRight (permute [2,1,3]))

  , assert "permute invalid [2,2,3]"
      (isLeft (permute [2,2,3]))

  , assert "timeWarp valid 1.5"
      (isRight (timeWarp 1.5))

  , assert "timeWarp invalid 0.0"
      (isLeft (timeWarp 0.0))

  -- Semigroup laws
  , assertEqual "identity <> t == t"
      (rotate Rotate90) (identity <> rotate Rotate90)

  , assertEqual "t <> identity == t"
      (rotate Rotate90) (rotate Rotate90 <> identity)

  , assertEqual "Monoid mempty == identity"
      Identity (mempty :: PretextTask)

  -- taskCount
  , assertEqual "taskCount identity == 0"
      0 (taskCount identity)

  , assertEqual "taskCount rotate == 1"
      1 (taskCount (rotate Rotate90))

  , assertEqual "taskCount seq == sum"
      2 (taskCount (rotate Rotate90 <> rotate Rotate180))

  -- isIdentity
  , assert "isIdentity Identity"
      (isIdentity identity)

  , assert "isIdentity Rotate0"
      (isIdentity (rotate Rotate0))

  , assert "not isIdentity Rotate90"
      (not (isIdentity (rotate Rotate90)))

  -- taskLabel
  , assertEqual "taskLabel rotate" "Rotate"    (taskLabel (rotate Rotate90))
  , assertEqual "taskLabel identity" "Identity" (taskLabel identity)
  ]

-- ---------------------------------------------------------------------------
-- Interpreter tests
-- ---------------------------------------------------------------------------

interpreterTests :: [TestResult]
interpreterTests =
  [ -- Identity is no-op
    assertEqual "runTask identity is no-op on seq"
      (Right seq8) (runTask identity seq8)

  , assertEqual "runTask identity is no-op on image"
      (Right img2x2) (runTask identity img2x2)

  -- Rotation
  , assertEqual "rotate Rotate0 is no-op"
      (Right img2x2) (runTask (rotate Rotate0) img2x2)

  , assertEqual "rotate 90 on 2x2"
      (Right (ImageSample [[3,1],[4,2]]))
      (runTask (rotate Rotate90) img2x2)

  , assertEqual "rotate 180 on 2x2"
      (Right (ImageSample [[4,3],[2,1]]))
      (runTask (rotate Rotate180) img2x2)

  , assertEqual "rotate 270 on 2x2"
      (Right (ImageSample [[2,4],[1,3]]))
      (runTask (rotate Rotate270) img2x2)

  -- Four 90-degree rotations = identity
  , assertEqual "four 90 rotations = identity"
      (Right img2x2)
      (runTask (rotate Rotate90 <> rotate Rotate90 <>
                rotate Rotate90 <> rotate Rotate90) img2x2)

  -- Rotation on sequence is rejected
  , assert "rotate rejected on sequence"
      (isLeft (runTask (rotate Rotate90) seq8))

  -- Permutation on sequence
  , assertEqual "permute [2,1] swaps first two of seq8"
      (Right (SeqSample [2,1,3,4,5,6,7,8]))
      (case permute [2,1] of
         Right p -> runTask p seq8
         Left  _ -> Left (UnsupportedOperation "build failed"))

  -- Permutation on image rows
  , assertEqual "permute [2,1] swaps rows of img2x2"
      (Right (ImageSample [[3,4],[1,2]]))
      (case permute [2,1] of
         Right p -> runTask p img2x2
         Left  _ -> Left (UnsupportedOperation "build failed"))

  -- Permutation out of range
  , assert "permute size > seq length is rejected"
      (isLeft $
        case permute [2,1,3,4,5,6,7,8,9] of
          Right p -> runTask p seq8
          Left  _ -> Left (UnsupportedOperation "build failed"))

  -- TimeWarp identity
  , assertEqual "timeWarp 1.0 is identity on seq"
      (Right seq8)
      (case timeWarp 1.0 of
         Right w -> runTask w seq8
         Left  _ -> Left (UnsupportedOperation "build failed"))

  -- TimeWarp compress
  , assertEqual "timeWarp 0.5 compresses seq8 to 4 elements"
      (Right (SeqSample [1,3,5,7]))
      (case timeWarp 0.5 of
         Right w -> runTask w seq8
         Left  _ -> Left (UnsupportedOperation "build failed"))

  -- TimeWarp output length
  , assertEqual "timeWarp 2.0 produces 16 elements from seq8"
      16
      (case timeWarp 2.0 of
         Right w -> case runTask w seq8 of
                      Right (SeqSample xs) -> length xs
                      _                    -> -1
         Left  _ -> -1)

  -- Pipeline
  , assert "rotate then permute on image succeeds"
      (isRight $
        case permute [2,1,3,4] of
          Right p -> runTask (rotate Rotate90 <> p) img4x4
          Left  _ -> Left (UnsupportedOperation "build failed"))
  ]

-- ---------------------------------------------------------------------------
-- Verifier tests
-- ---------------------------------------------------------------------------

verifierTests :: [TestResult]
verifierTests =
  [ -- Static: all-identity pipeline has warning
    assert "static: all-identity pipeline flagged"
      (not . null . staticErrors $ verifyStatic identity)

  -- Static: valid rotate task passes
  , assert "static: rotate90 passes"
      (null . staticErrors $ verifyStatic (rotate Rotate90))

  -- Dynamic: permutation out of range
  , assert "dynamic: perm size > seq len is error"
      (case permute [4,3,2,1] of
         Right p -> not . null . dynamicErrors $ verifyDynamic p (SeqInfo 3)
         Left  _ -> False)

  -- Dynamic: rotation on sequence is error
  , assert "dynamic: rotate on SeqInfo is error"
      (not . null . dynamicErrors $
         verifyDynamic (rotate Rotate90) (SeqInfo 8))

  -- Dynamic: rotation on image passes
  , assert "dynamic: rotate on ImageInfo passes"
      (null . dynamicErrors $
         verifyDynamic (rotate Rotate90) (ImageInfo 4 4))

  -- Dynamic: permutation fits in image rows
  , assert "dynamic: perm fits in ImageInfo rows passes"
      (case permute [2,1,3,4] of
         Right p -> null . dynamicErrors $ verifyDynamic p (ImageInfo 4 4)
         Left  _ -> False)

  -- Combined verify on valid pipeline
  , assert "isClean: valid rotate+permute on ImageInfo"
      (case permute [2,1,3,4] of
         Right p -> isClean $ verify (rotate Rotate90 <> p) (ImageInfo 4 4)
         Left  _ -> False)

  -- prettyReport on clean report
  , assert "prettyReport clean contains 'Verification passed'"
      (isSubStr "Verification passed"
        (prettyReport (VerificationReport [] [])))
  ]

-- ---------------------------------------------------------------------------
-- Optimizer tests
-- ---------------------------------------------------------------------------

optimizerTests :: [TestResult]
optimizerTests =
  [ -- R1: identity elimination
    assertEqual "opt R1: identity <> t -> t"
      (rotate Rotate90) (optimize (identity <> rotate Rotate90))

  , assertEqual "opt R1: t <> identity -> t"
      (rotate Rotate90) (optimize (rotate Rotate90 <> identity))

  -- R2: rotation fusion
  , assertEqual "opt R2: rotate90 + rotate180 -> rotate270"
      (Rotate Rotate270) (optimize (rotate Rotate90 <> rotate Rotate180))

  -- R3: rotation cancellation
  , assertEqual "opt R3: rotate90 + rotate270 -> identity"
      Identity (optimize (rotate Rotate90 <> rotate Rotate270))

  , assertEqual "opt: four rotate90 -> identity"
      Identity
      (optimize (rotate Rotate90 <> rotate Rotate90 <>
                 rotate Rotate90 <> rotate Rotate90))

  -- R5: timeWarp fusion
  , assertEqual "opt R5: timeWarp(2.0) * timeWarp(0.5) -> identity"
      Identity
      (optimize $
         fromRight identity (timeWarp 2.0) <>
         fromRight identity (timeWarp 0.5))

  -- R6: timeWarp(1.0) -> identity
  , assertEqual "opt R6: timeWarp(1.0) -> identity"
      Identity (optimize (fromRight identity (timeWarp 1.0)))

  -- R4: permutation self-inverse
  , assertEqual "opt R4: permute([2,1]) <> permute([2,1]) -> identity"
      Identity
      (optimize $
         fromRight identity (permute [2,1]) <>
         fromRight identity (permute [2,1]))

  -- No regression: non-fusible pair stays as two tasks
  , assert "opt: rotate(90) + permute not fused (different types)"
      (case permute [2,1,3] of
         Right p ->
           let opt = optimize (rotate Rotate90 <> p)
           in taskCount opt == 2
         Left _ -> False)

  -- fuseRotations exported function
  , assertEqual "fuseRotations: rotate90 + rotate90"
      (Rotate Rotate180) (fuseRotations (Seq (Rotate Rotate90) (Rotate Rotate90)))

  -- eliminateIdentity exported function
  , assertEqual "eliminateIdentity: identity <> t"
      (Rotate Rotate90) (eliminateIdentity (Seq Identity (Rotate Rotate90)))

  -- optimizationSteps: non-empty and terminates
  , assert "optimizationSteps terminates with at least 1 step"
      (let steps = optimizationSteps
                     (rotate Rotate90 <> rotate Rotate270 <>
                      fromRight identity (timeWarp 1.0))
       in not (null steps))

  -- optimizationSteps: last step is fixed point
  , assert "optimizationSteps: last step is fixed point"
      (let steps = optimizationSteps
                     (rotate Rotate90 <> rotate Rotate270)
           final = last steps
       in optimize final == final)
  ]

-- ---------------------------------------------------------------------------
-- Pretty print tests
-- ---------------------------------------------------------------------------

prettyTests :: [TestResult]
prettyTests =
  [ assertEqual "prettyInfix identity"
      "identity" (prettyInfix identity)

  , assertEqual "prettyInfix rotate90"
      "rotate(90\176)" (prettyInfix (rotate Rotate90))

  , assert "prettyInfix Seq contains right-arrow separator (U+2192)"
      ('\8594' `elem` prettyInfix (rotate Rotate90 <> rotate Rotate180))

  , assert "prettyJSON contains type field"
      (isSubStr "\"type\"" (prettyJSON (rotate Rotate90)))

  , assert "prettyJSON rotate contains rotate"
      (isSubStr "rotate" (prettyJSON (rotate Rotate90)))

  , assert "prettyTree is multi-line for Seq"
      (length (lines (prettyTree (rotate Rotate90 <> rotate Rotate180))) > 1)

  , assert "prettyTree contains Seq"
      (isSubStr "Seq" (prettyTree (rotate Rotate90 <> rotate Rotate180)))

  , assert "prettyLog contains Step"
      (isSubStr "Step" (prettyLog (rotate Rotate90)))

  , assert "prettyDataSample seq starts with Sequence"
      (isSubStr "Sequence" (prettyDataSample seq8))

  , assert "prettyDataSample image starts with Image"
      (isSubStr "Image" (prettyDataSample img2x2))

  , assert "prettyReport on VerificationReport with errors is non-empty"
      (not . null $ prettyReport
         (VerificationReport [UnsupportedOperation "test"] []))
  ]

-- ---------------------------------------------------------------------------
-- Generator tests
-- ---------------------------------------------------------------------------

generatorTests :: [TestResult]
generatorTests =
  [ assert "sampleTasks produces exactly n tasks"
      (length (sampleTasks 42 10) == 10)

  , assert "sampleTasks with 0 tasks produces empty list"
      (null (sampleTasks 42 0))

  , assert "different seeds produce different tasks"
      (sampleTasks 1 3 /= sampleTasks 999 3)

  , assert "randomPipeline of length 0 == identity"
      (let (t, _) = runGen (randomPipeline 0 4) 123
       in t == identity)

  , assert "generated tasks can be shown (non-empty string)"
      (not . null . show $ head (sampleTasks 7 1))

  , assert "generated tasks pass static verifier (no hard errors)"
      (all (\t -> null
              [ e | e@(DimensionMismatch _) <- staticErrors (verifyStatic t)])
           (sampleTasks 42 20))

  , assert "optimizing generated tasks terminates"
      (all (\t -> let o = optimize t in o == optimize o)
           (sampleTasks 99 10))
  ]

-- ---------------------------------------------------------------------------
-- Edge case tests
-- ---------------------------------------------------------------------------

edgeCaseTests :: [TestResult]
edgeCaseTests =
  [ -- Single-element sequence
    assertEqual "permute [1] on single-element seq is identity"
      (Right (SeqSample [42.0 :: Double]))
      (case permute [1] of
         Right p -> runTask p (SeqSample [42.0])
         Left  _ -> Left (UnsupportedOperation "build failed"))

  -- Empty permutation is rejected
  , assert "empty permutation is rejected"
      (isLeft (permute ([] :: [Int])))

  -- 1x1 image rotation
  , assertEqual "rotate 90 on 1x1 image is no-op"
      (Right (ImageSample [[99.0 :: Double]]))
      (runTask (rotate Rotate90) (ImageSample [[99.0]]))

  -- Deep pipeline: 10 x rotate90 = rotate(900) = rotate(180)
  , assert "10 x rotate90 optimises to rotate180"
      (let deep = foldr (<>) identity (replicate 10 (rotate Rotate90))
       in optimize deep == Rotate Rotate180)

  -- Parallel task succeeds
  , assert "parallel task applies correctly"
      (isRight (runTask
         (parallel (rotate Rotate90) (rotate Rotate180)) img2x2))

  -- Parallel on sequence: left is rotation -> fails
  , assert "parallel: left rotation on seq fails"
      (isLeft (runTask
         (parallel (rotate Rotate90) (rotate Rotate180)) seq8))

  -- mconcat of empty list = identity
  , assertEqual "mconcat [] == identity"
      Identity (mconcat ([] :: [PretextTask]))

  -- taskDepth of leaf = 1
  , assertEqual "taskDepth of leaf == 1"
      1 (taskDepth (rotate Rotate90))

  -- taskDepth of identity = 0
  , assertEqual "taskDepth of identity == 0"
      0 (taskDepth identity)

  -- Sequential task depth
  , assertEqual "taskDepth of Seq = 1 + max depth"
      2 (taskDepth (rotate Rotate90 <> rotate Rotate180))
  ]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  putStrLn "========================================================="
  putStrLn "       Pretext Task Factory -- Test Suite"
  putStrLn "========================================================="

  runTests
    [ ("Types",       typesTests)
    , ("DSL",         dslTests)
    , ("Interpreter", interpreterTests)
    , ("Verifier",    verifierTests)
    , ("Optimizer",   optimizerTests)
    , ("PrettyPrint", prettyTests)
    , ("Generator",   generatorTests)
    , ("Edge Cases",  edgeCaseTests)
    ]
