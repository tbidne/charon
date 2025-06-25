{-# LANGUAGE CPP #-}

-- | Tests for d command.
module Integration.Commands.Delete
  ( tests,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend
import Charon.Exception (RootE)
import Data.Text qualified as T
import Integration.Prelude

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
      deletesRootError b "g:   "
      -- Cannot actually test "  g:  " i.e. leading whitespace on windows
      -- because it is not a valid path with the colon.
    ]
#else
    [ deletesRootError b "/",
      deletesRootError b "/   ",
      deletesRootError b " /   "
    ]
#endif

deletesRootError :: Backend -> String -> TestTree
deletesRootError b r = testCase ("delete '" <> r <> "'") $ do
  (ex, terminal, deletedPaths) <- captureCharonIntExceptionPure @RootE argList

  "Attempted to delete root! This is not allowed." @=? ex
  assertMatch expected (T.strip terminal)
  "[]" @=? deletedPaths
  where
    expected = Exact ""

    argList :: [String]
    argList = "delete" : r : ["-t", "/dev/null", "--backend", Backend.backendName b]
