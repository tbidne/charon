{-# LANGUAGE CPP #-}

-- | Tests for d command.
--
-- @since 0.1
module Integration.Commands.D
  ( tests,
  )
where

import Data.Text qualified as T
import Integration.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Delete (d)"
    [ testRoot
    ]

testRoot :: TestTree
testRoot =
  testGroup
    "Delete root throws error"
#if WINDOWS
    [ deletesRootError "C:\\",
      deletesRootError "d:",
      deletesRootError "X:\\   ",
      deletesRootError "  g:   "
    ]
#else
    [ deletesRootError "/",
      deletesRootError "/   ",
      deletesRootError " /   "
    ]
#endif

deletesRootError :: String -> TestTree
deletesRootError r = testCase ("delete '" <> r <> "'") $ do
  (ex, terminal, deletedPaths) <- captureSafeRmIntExceptionPure @ExitCode argList

  "ExitFailure 1" @=? ex
  errMsg @=? terminal
  "" @=? deletedPaths
  where
    errMsg :: Text
    errMsg =
      "Error deleting path '"
        <> T.pack r
        <> "': Attempted to delete root! This is not allowed.\n"

    argList :: [String]
    argList = "d" : r : ["-t", "/dev/null"]
