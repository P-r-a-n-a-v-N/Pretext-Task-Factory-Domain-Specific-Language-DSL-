# 🧩 Pretext Task Factory

> **A Domain-Specific Language for Defining, Composing, and Verifying Self-Supervised Learning Pretext Tasks**

[![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=flat&logo=haskell&logoColor=white)](https://www.haskell.org)
[![GHC](https://img.shields.io/badge/GHC-9.4%2B-blue)](https://www.haskell.org/ghc/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Build](https://img.shields.io/badge/build-passing-brightgreen)]()

---

## 📖 Table of Contents

1. [What Are SSL Pretext Tasks?](#what-are-ssl-pretext-tasks)
2. [Project Overview](#project-overview)
3. [DSL Design Philosophy](#dsl-design-philosophy)
4. [Architecture](#architecture)
5. [Quick Start](#quick-start)
6. [DSL Usage Guide](#dsl-usage-guide)
7. [Module Reference](#module-reference)
8. [Example Runs](#example-runs)
9. [Running Tests](#running-tests)
10. [Design Decisions](#design-decisions)
11. [Project Structure](#project-structure)

---

## What Are SSL Pretext Tasks?

**Self-Supervised Learning (SSL)** is a machine learning paradigm where a model learns representations from unlabelled data by solving *pretext tasks* — surrogate objectives that require understanding the structure of the data without human annotation.

Common pretext tasks include:

| Task | Description | Application |
|------|-------------|-------------|
| **Rotation** | Predict the rotation angle of an image | Image understanding |
| **Permutation** | Recover the original order of shuffled patches | Spatial reasoning |
| **TimeWarp** | Detect or invert temporal distortion | Time-series analysis |

The problem is that in practice these tasks are **hardcoded** — each implementation is a one-off function with no shared abstraction, no composability, and no invariant checking. This project solves that.

---

## Project Overview

**Pretext Task Factory** is an *embedded domain-specific language (EDSL)* in Haskell that provides:

- ✅ A **declarative, composable** way to define pretext transformation pipelines
- ✅ **Smart constructors** that validate all parameters at construction time
- ✅ An **interpreter** that executes pipelines on images and sequences
- ✅ A **verification engine** with static and dynamic checks
- ✅ An **algebraic optimizer** that simplifies pipelines using rewrite rules
- ✅ A **random generator** for producing valid test programs
- ✅ **Multiple pretty-printing** formats for debugging and logging

All of this with **zero runtime dependencies** — only GHC's base library.

---

## DSL Design Philosophy

### 1. Make Illegal States Unrepresentable

Every value that can be invalid is wrapped in a `newtype` with a **smart constructor** that validates it:

```haskell
-- You cannot construct an invalid permutation:
mkPermutation [1,1,3]   -- Left (InvalidPermutation "Duplicate indices...")
mkPermutation [2,1,3]   -- Right (Permutation {rawIndices = [1,0,2]})

-- You cannot construct an invalid warp factor:
mkWarpFactor (-0.5)     -- Left (InvalidWarpFactor "Must be strictly positive...")
mkWarpFactor 1.5        -- Right (WarpFactor {rawFactor = 1.5})
```

### 2. Algebraic Composition via Semigroup/Monoid

`PretextTask` is a `Semigroup` where `<>` means sequential composition, and a `Monoid` with `identity` as the neutral element. This gives us the full algebraic power:

```haskell
-- These are all equivalent:
rotate Rotate90 <> rotate Rotate270   -- optimizes to identity
mconcat [rotate Rotate90, rotate Rotate90, rotate Rotate90, rotate Rotate90]
mempty  -- identity
```

### 3. Initial Algebra Encoding

We use an **initial encoding** (ADT) rather than a final-tagless approach. This means:
- Multiple interpreters can pattern-match the same tree
- The optimizer rewrites the tree before execution
- Pretty-printers, verifiers, and interpreters are all independent

### 4. Functional Purity

There is **no IO** in any module except `Main.hs`. Every computation is a pure function, making the DSL easy to test, reason about, and compose.

---

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                        User Code                         │
│   rotate Rotate90 <> permute [2,1,3] <> timeWarp 1.5    │
└──────────────────────┬──────────────────────────────────┘
                       │ constructs
                       ▼
┌─────────────────────────────────────────────────────────┐
│                     DSL.hs                               │
│   PretextTask ADT  •  Smart Constructors  •  Monoid      │
└──────┬───────────────────┬──────────────────────────────┘
       │                   │
       ▼                   ▼
┌─────────────┐   ┌────────────────┐   ┌──────────────────┐
│ Verifier.hs │   │ Optimizer.hs   │   │ PrettyPrint.hs   │
│             │   │                │   │                   │
│ Static      │   │ 7 rewrite rules│   │ Infix / Tree /   │
│ Dynamic     │   │ Fixed-point    │   │ JSON / Log        │
└─────────────┘   └───────┬────────┘   └──────────────────┘
                          │ optimized tree
                          ▼
                ┌─────────────────────┐
                │  Interpreter.hs     │
                │                     │
                │  applyRotation      │
                │  applyPermutation   │
                │  applyTimeWarp      │
                └─────────────────────┘
                          │
              ┌───────────┴──────────────┐
              ▼                          ▼
    ImageSample [[...]]          SeqSample [...]
```

---

## Quick Start

### Prerequisites

- GHC ≥ 9.4 (tested up to 9.8)
- Cabal ≥ 3.8

Install via [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.4
ghcup install cabal 3.10.2.0
```

### Build & Run

```bash
git clone https://github.com/yourusername/pretext-task-factory.git
cd pretext-task-factory

# Build everything
cabal build all

# Run the demonstration
cabal run pretext-task-factory

# Run the test suite
cabal test --test-show-details=always
```

### REPL

```bash
cabal repl

# In GHCi:
:m Types DSL Interpreter Optimizer PrettyPrint
let task = rotate Rotate90 <> fromRight identity (permute [2,1,3])
prettyInfix task
prettyInfix (optimize task)
runTask task (SeqSample [1.0, 2.0, 3.0 :: Double])
```

---

## DSL Usage Guide

### Building Tasks

```haskell
import Types
import DSL

-- Leaf tasks (always valid):
let r   = rotate Rotate90          -- :: PretextTask
let r2  = rotate Rotate180
let rid = rotate Rotate0           -- same as identity

-- Fallible leaf tasks (smart constructor returns Either):
Right p <- return $ permute [2,1,3]       -- :: PretextTask
Right w <- return $ timeWarp 1.5          -- :: PretextTask

-- Invalid constructions are rejected at build time:
Left err = permute [1,1,3]       -- duplicate indices
Left err = timeWarp (-1.0)       -- negative factor
Left err = permute []            -- empty permutation
```

### Composing Pipelines

```haskell
-- Sequential: apply left, then right
let pipeline = rotate Rotate90 <> p <> w
-- Equivalent to: sequential (sequential (rotate Rotate90) p) w

-- Parallel: apply both to the same input independently (multi-view SSL)
let multiView = parallel (rotate Rotate90) (rotate Rotate270)

-- Monoid accumulation:
let pipe = mconcat [ rotate Rotate90
                   , fromRight identity (permute [2,1,3])
                   , fromRight identity (timeWarp 0.8)
                   ]
```

### Running Tasks

```haskell
import Interpreter

let img = ImageSample [[1,2],[3,4]] :: DataSample Double
let seq = SeqSample [1..8]          :: DataSample Double

-- Execute on an image:
runTask (rotate Rotate90) img
-- Right (ImageSample [[3,1],[4,2]])

-- Execute on a sequence:
runTask (fromRight identity (permute [2,1,3,4,5,6,7,8])) seq
-- Right (SeqSample [2.0,1.0,3.0,4.0,5.0,6.0,7.0,8.0])

-- Execute a pipeline:
let pipeline = rotate Rotate90 <> fromRight identity (permute [2,1])
runTask pipeline img
```

### Verification

```haskell
import Verifier

-- Static check (no data needed):
let report = verifyStatic myTask
isClean report  -- True if no errors

-- Dynamic check (requires data dimensions):
let report = verify myTask (ImageInfo 4 4)
prettyReport report
```

### Optimization

```haskell
import Optimizer

-- Rotate 90° then 270° cancels out:
optimize (rotate Rotate90 <> rotate Rotate270)
-- Identity

-- Fuse consecutive time-warps:
optimize (fromRight identity (timeWarp 2.0) <> fromRight identity (timeWarp 0.5))
-- Identity  (2.0 × 0.5 = 1.0 → identity)

-- See every step:
optimizationSteps myTask
-- [step0, step1, ..., fixedPoint]
```

### Pretty Printing

```haskell
import PrettyPrint

let task = rotate Rotate90 <> fromRight identity (permute [2,1,3])

-- Pipeline notation:
prettyInfix task
-- "rotate(90°) → permute([2,1,3])"

-- Tree view:
putStr (prettyTree task)
-- Seq
--   ├─ rotate(90°)
--   └─ permute([2,1,3])

-- JSON:
putStr (prettyJSON task)

-- Execution log:
putStr (prettyLog task)
-- Execution trace:
--   Step 1: Apply rotation 90° (clockwise)
--   Step 2: Apply permutation [2,1,3]
```

### Random Generation

```haskell
import Generator

-- Generate 5 random tasks from seed 42:
let tasks = sampleTasks 42 5
mapM_ (putStrLn . prettyInfix) tasks
```

---

## Module Reference

| Module | Responsibility |
|--------|---------------|
| `Types` | Core data types: `DataSample`, `RotationAngle`, `Permutation`, `WarpFactor`, error types |
| `DSL` | `PretextTask` ADT, smart constructors, `Semigroup`/`Monoid`, inspection helpers |
| `Interpreter` | Execute tasks on `DataSample` values; rotation/permutation/time-warp engines |
| `Verifier` | Static and dynamic verification; `VerificationReport` |
| `Optimizer` | 7 algebraic rewrite rules; fixed-point simplification |
| `PrettyPrint` | Infix, tree, JSON, and log renderers |
| `Generator` | Dependency-free pseudo-random DSL program generator |

---

## Example Runs

### Basic pipeline

```
$ cabal run pretext-task-factory

╔═══════════════════════════════════════════════════════════════╗
║         Pretext Task Factory — DSL Demonstration             ║
╚═══════════════════════════════════════════════════════════════╝

══════════════════════════════════════════════════════════════════
  Demo 1 — Basic Transformations on an Image
══════════════════════════════════════════════════════════════════

  ── Original image:
  Image [
    [0.1, 0.2, 0.3, 0.4]
    [0.5, 0.6, 0.7, 0.8]
    [0.9, 1.0, 0.0, 0.1]
    [0.2, 0.3, 0.4, 0.5]
  ]

  ── rotate(90°):
  DSL expression : rotate(90°)
  Verify (static): ✓ Verification passed — no errors found.
  Image [
    [0.2, 0.9, 0.5, 0.1]
    [0.3, 1.0, 0.6, 0.2]
    [0.4, 0.0, 0.7, 0.3]
    [0.5, 0.1, 0.8, 0.4]
  ]
```

### Optimizer in action

```
══════════════════════════════════════════════════════════════════
  Demo 5 — Optimizer / Algebraic Simplification
══════════════════════════════════════════════════════════════════

  ── Rule R2+R3: rotate(90°) → rotate(270°) → identity
  Before: rotate(90°) → rotate(270°)
  After : identity

  ── Rule R5: timeWarp(2.0) → timeWarp(0.5) → identity
  Before: timeWarp(2.0) → timeWarp(0.5)
  After : identity

  ── Optimisation trace (step-by-step):
  Input: rotate(90°) → rotate(270°) → identity → rotate(180°)
  Step 0: rotate(90°) → rotate(270°) → identity → rotate(180°)
  Step 1: identity → rotate(180°)
  Step 2: rotate(180°)
```

### Test suite output

```
$ cabal test --test-show-details=always

╔═════════════════════════════════════════════════════════╗
║       Pretext Task Factory — Test Suite                 ║
╚═════════════════════════════════════════════════════════╝

▸ Types
  ✓ mkPermutation valid [2,1,3]
  ✓ mkPermutation rejects empty
  ✓ mkPermutation rejects duplicates [1,1,3]
  ...

▸ Optimizer
  ✓ opt: rotate90 + rotate270 = identity
  ✓ opt: timeWarp(2.0) + timeWarp(0.5) = identity
  ✓ opt: permute([2,1]) + permute([2,1]) = identity
  ...

────────────────────────────────────────────────────────────
Results: 60/60 passed
✓ All tests passed!
```

---

## Design Decisions

### Why an Initial Encoding?

We chose the *initial* (ADT-based) encoding over *final tagless* because:
1. The optimizer needs to pattern-match on the tree structure
2. Serialisation to JSON / pretty-print formats is natural
3. Equality (`Eq`) is derivable, which simplifies tests

### Why No External Dependencies?

Zero external dependencies means:
- `cabal install` works immediately with any GHC ≥ 9.4
- No version conflicts in a student or research environment
- The random generator uses a hand-rolled LCG (standard for DSP/ML work)

### Optimizer Correctness

All rewrite rules are *semantics-preserving*:

| Rule | Law |
|------|-----|
| R1 | `id ∘ f = f ∘ id = f` (category identity) |
| R2 | `rotₐ ∘ rotᵦ = rot₍ₐ₊ᵦ₎ mod 360` (rotation group) |
| R3 | `rot₃₆₀ = id` |
| R4 | `permₚ ∘ permᵩ = perm₍ₚ∘ᵩ₎` (permutation group) |
| R5 | `warpᶠ ∘ warpᵍ = warp₍ᶠ×ᵍ₎` (multiplicative monoid) |
| R6 | `warp₁.₀ = id` |
| R7 | `permₚ ∘ permₚ₋₁ = id` (inverse in permutation group) |

---

## Project Structure

```
PretextTaskFactory/
├── app/
│   └── Main.hs               ← 9-section interactive demo
├── src/
│   ├── Types.hs              ← Core data types & smart constructors
│   ├── DSL.hs                ← PretextTask ADT & Semigroup/Monoid
│   ├── Interpreter.hs        ← Execution engine
│   ├── Verifier.hs           ← Static & dynamic verification
│   ├── Optimizer.hs          ← Algebraic rewrite rules
│   ├── PrettyPrint.hs        ← Multiple rendering formats
│   └── Generator.hs          ← Pseudo-random DSL program generator
├── test/
│   └── Spec.hs               ← 60+ pure test cases
├── pretext-task-factory.cabal
├── CHANGELOG.md
├── LICENSE
└── README.md
```

---

## Contributing

Pull requests are welcome. Please ensure:
1. All tests pass: `cabal test`
2. No new GHC warnings: `cabal build -Wall`
3. New features include tests

---

## License

MIT — see [LICENSE](LICENSE).
