-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.Data.Index.Default qualified as Data.Index.Default
import Unit.Data.Index.Fdo qualified as Data.Index.Fdo
import Unit.Data.PathData qualified as Data.PathData
import Unit.Data.UniqueSeq qualified as Data.UniqueSeq
import Unit.Prelude
import Unit.Runner qualified as Runner
import Unit.Trash qualified as Trash
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
        [ Data.Index.Default.tests,
          Data.Index.Fdo.tests,
          Data.PathData.tests,
          Data.UniqueSeq.tests,
          Runner.tests,
          Trash.tests,
          Utils.tests
        ]
