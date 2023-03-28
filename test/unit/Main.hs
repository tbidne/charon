-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.Data.Index qualified as Data.Index
import Unit.Data.PathData qualified as Data.PathData
import Unit.Data.Trash qualified as Trash
import Unit.Data.UniqueSeq qualified as Data.UniqueSeq
import Unit.Prelude
import Unit.Runner qualified as Runner
import Unit.Utils qualified as Utils

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.localOption OnPass $
      testGroup
        "Unit Tests"
        [ Data.Index.tests,
          Data.PathData.tests,
          Data.UniqueSeq.tests,
          Trash.tests,
          Runner.tests,
          Utils.tests
        ]
