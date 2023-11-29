-- | Entrypoint for unit tests.
module Main (main) where

import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.Backend.Cbor.PathData qualified as Backend.Cbor.PathData
import Unit.Backend.Default.Trash qualified as Backend.Default.Trash
import Unit.Backend.Default.Utils qualified as Backend.Default.Utils
import Unit.Backend.Fdo.PathData qualified as Backend.Fdo.PathData
import Unit.Backend.Json.PathData qualified as Backend.Json.PathData
import Unit.Data.Index qualified as Data.Index
import Unit.Data.UniqueSeq qualified as Data.UniqueSeq
import Unit.Prelude
import Unit.Runner qualified as Runner
import Unit.Utils qualified as Utils

-- | Runs unit tests.
main :: IO ()
main =
  Tasty.defaultMain
    $ Tasty.localOption OnPass
    $ testGroup
      "Unit Tests"
      [ Backend.Cbor.PathData.tests,
        Backend.Default.Utils.tests,
        Backend.Fdo.PathData.tests,
        Backend.Json.PathData.tests,
        Backend.Default.Trash.tests,
        Data.Index.tests,
        Data.UniqueSeq.tests,
        Runner.tests,
        Utils.tests
      ]
