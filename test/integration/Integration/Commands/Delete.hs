{-# LANGUAGE CPP #-}

-- | Tests for d command.
module Integration.Commands.Delete
  ( tests,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend
import Charon.Exception (DotsPathE, EmptyPathE, RootE)
import Data.Text qualified as T
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "Delete (d)"
    (mkTests <$> [minBound .. maxBound])
  where
    mkTests b =
      testGroup
        (Backend.backendTestDesc b)
        [ testRoot b,
          testDeletesEmptyError b,
          testDeletesDotsError1 b,
          testDeletesDotsError2 b,
          testDeletesDotsError3 b
        ]

testRoot :: Backend -> TestTree
testRoot b =
  testGroup
    "Delete root throws error"
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

testDeletesEmptyError :: Backend -> TestTree
testDeletesEmptyError b = testCase "Delete empty path fails" $ do
  (ex, terminal, deletedPaths) <- captureCharonIntExceptionPure @EmptyPathE args
  "Attempted to delete the empty path! This is not allowed." @=? ex
  assertMatch (Exact "") (T.strip terminal)
  "[]" @=? deletedPaths
  where
    args = ["delete", "", "-t", "/dev/null", "--backend", Backend.backendName b]

testDeletesDotsError1 :: Backend -> TestTree
testDeletesDotsError1 b = testCase "Delete '.' fails" $ do
  (ex, terminal, deletedPaths) <- captureCharonIntExceptionPure @DotsPathE args
  "Attempted to delete the special path '.'! This is not allowed." @=? ex
  assertMatch (Exact "") (T.strip terminal)
  "[]" @=? deletedPaths
  where
    args = ["delete", ".", "-t", "/dev/null", "--backend", Backend.backendName b]

testDeletesDotsError2 :: Backend -> TestTree
testDeletesDotsError2 b = testCase "Delete '..' fails" $ do
  (ex, terminal, deletedPaths) <- captureCharonIntExceptionPure @DotsPathE args
  "Attempted to delete the special path '..'! This is not allowed." @=? ex
  assertMatch (Exact "") (T.strip terminal)
  "[]" @=? deletedPaths
  where
    args = ["delete", "..", "-t", "/dev/null", "--backend", Backend.backendName b]

testDeletesDotsError3 :: Backend -> TestTree
testDeletesDotsError3 b = testCase "Delete 'tmp/...' fails" $ do
  (ex, terminal, deletedPaths) <- captureCharonIntExceptionPure @DotsPathE args
  "Attempted to delete the special path 'tmp/...'! This is not allowed." @=? ex
  assertMatch (Exact "") (T.strip terminal)
  "[]" @=? deletedPaths
  where
    args = ["delete", "tmp/...", "-t", "/dev/null", "--backend", Backend.backendName b]
