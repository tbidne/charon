-- | Prelude for benchmarks.
module Benchmarks.Prelude
  ( module X,
  )
where

import Charon.Prelude as X
import Test.Tasty.Bench as X
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )
import Test.Utils as X (clearDirectory)
