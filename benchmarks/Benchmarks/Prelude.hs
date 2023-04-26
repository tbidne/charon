-- | Prelude for benchmarks.
module Benchmarks.Prelude
  ( module X,
    header,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import SafeRm.Data.Backend (Backend (..))
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
header :: ByteString
header = encodeUtf8 . T.pack . L.intercalate "," $ headerNames BackendDefault
