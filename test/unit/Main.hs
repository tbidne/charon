-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Data.Index qualified as Index
import Unit.Data.PathData qualified as PathData
import Unit.Data.UniqueSeq qualified as UniqueSeq
import Unit.Prelude
import Unit.Runner qualified as Runner

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    testGroup
      "Unit Tests"
      [ Index.tests,
        PathData.tests,
        UniqueSeq.tests,
        Runner.tests
      ]
