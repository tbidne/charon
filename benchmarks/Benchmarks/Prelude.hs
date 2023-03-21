-- | Prelude for benchmarks.
--
-- @since 0.1
module Benchmarks.Prelude
  ( module X,
    header,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.List qualified as L
import SafeRm.Data.PathData (headerNames)
import SafeRm.Prelude as X
import Test.Tasty.Bench as X
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )
import Test.Utils as X (clearDirectory)

-- | Csv header.
--
-- @since 0.1
header :: ByteString
header = Char8.pack $ L.intercalate "," headerNames
