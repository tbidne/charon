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
deletesRootError = testCase "Delete root throws error" $ do
  let argList = ["d", "/", "-t", "/dev/null"]

  (ex, terminal, deletedPaths) <- captureSafeRmIntExceptionPure @ExitCode argList

  "ExitFailure 1" @=? ex
  "Error deleting path '/': Attempted to delete root! This is not allowed.\n" @=? terminal
  "" @=? deletedPaths
