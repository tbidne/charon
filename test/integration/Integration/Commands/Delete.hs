{-# LANGUAGE CPP #-}

-- | Tests for d command.
module Integration.Commands.Delete
  ( tests,
  )
where

import Data.Text qualified as T
import Integration.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Exception (RootE)

tests :: TestTree
tests =
  testGroup
    "Delete (d)"
    (testRoot <$> [minBound .. maxBound])

testRoot :: Backend -> TestTree
testRoot b =
  testGroup
    ("Delete root throws error " ++ Backend.backendTestDesc b)
#if WINDOWS
    [ deletesRootError b "C:\\",
      deletesRootError b "d:",
      deletesRootError b "X:\\   ",
      deletesRootError b "  g:   "
    ]
#else
    [ deletesRootError b "/",
      deletesRootError b "/   ",
      deletesRootError b " /   "
    ]
#endif

deletesRootError :: Backend -> String -> TestTree
deletesRootError b r = testCase ("delete '" <> r <> "'") $ do
  (ex, terminal, deletedPaths) <- captureSafeRmIntExceptionPure @RootE argList

  "Attempted to delete root! This is not allowed." @=? ex
  errMsg @=? terminal
  "" @=? deletedPaths
  where
    errMsg :: Text
    errMsg =
      "Error deleting path '"
        <> T.pack r
        <> "': Attempted to delete root! This is not allowed.\n"

    argList :: [String]
    argList = "delete" : r : ["-t", "/dev/null", "--backend", Backend.backendArg b]
