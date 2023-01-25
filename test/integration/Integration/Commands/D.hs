-- | Tests for d command.
--
-- @since 0.1
module Integration.Commands.D
  ( tests,
  )
where

import Integration.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Delete (d)"
    [ deletesRootError
    ]

deletesRootError :: TestTree
deletesRootError = goldenVsStringDiff desc diff gpath $ do
  let argList = ["d", "/", "-t", "/dev/null"]

  (ex, terminal, deletedPaths) <-
    captureSafeRmIntExceptionPure
      @ExitCode
      "DELETE"
      argList

  pure $ capturedToBs [ex, terminal, deletedPaths]
  where
    desc = "Delete root throws error"
    gpath = goldenPath </> "root-error.golden"

goldenPath :: FilePath
goldenPath = "test/integration/Integration/Commands/D"
