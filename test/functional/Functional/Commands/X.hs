-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Permanent Delete (x)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deletesSome args,
      deletesNoForce args,
      deletesWildcards args,
      deletesSomeWildcards args
    ]

deletesOne :: IO FilePath -> TestTree
deletesOne args = testCase "Permanently deletes a single file" $ do
  testDir <- getTestPath args "deletesOne"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  let permDelArgList = ["x", "f1", "-f", "-t", trashDir]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ f1 : mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.singleton
        (mkPathData PathTypeFile "f1" "/safe-rm/functional/x/deletesOne/f1")

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = mempty

deletesMany :: IO FilePath -> TestTree
deletesMany args = testCase "Permanently deletes several paths" $ do
  testDir <- getTestPath args "deletesMany"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  -- leave f2 alone
  let permDelArgList = ["x", "f1", "f3", "dir1", "dir2", "-f", "-t", trashDir]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesToDelete
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2/dir3"]
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/x/deletesMany/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/x/deletesMany/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/x/deletesMany/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/x/deletesMany/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/x/deletesMany/dir2"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.singleton
        (mkPathData PathTypeFile "f2" "/safe-rm/functional/x/deletesMany/f2")
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deleteUnknownError :: IO FilePath -> TestTree
deleteUnknownError args = testCase "Delete unknown prints error" $ do
  testDir <- getTestPath args "deleteUnknownError"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a permanent delete, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the perm safe-rm
  -- failure only.
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE
  let permDelArgList = ["x", "bad file", "-f", "-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- assert exception
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet permDelIdxSet
  delExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/x/deleteUnknownError/f1"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesSome :: IO FilePath -> TestTree
deletesSome args = testCase "Deletes some, errors on others" $ do
  testDir <- getTestPath args "deletesSome"
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = ("d" : realFiles) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  -- delete to trash first
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE
  let permDelArgList =
        ("x" : filesTryPermDelete) <> ["-f", "-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesTryPermDelete

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/x/deletesSome/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/x/deletesSome/f2",
          mkPathData PathTypeFile "f5" "/safe-rm/functional/x/deletesSome/f5"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = mempty

deletesNoForce :: IO FilePath -> TestTree
deletesNoForce args = testCase "Permanently deletes several paths without --force" $ do
  testDir <- getTestPath args "deletesNoForce"
  let trashDir = testDir </> ".trash"
      fileDeleteNames = show @Int <$> [1 .. 5]
      fileDeletePaths = (testDir </>) <$> fileDeleteNames
      delArgList = ("d" : fileDeletePaths) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  createFiles fileDeletePaths
  assertFilesExist fileDeletePaths

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames
  assertFilesDoNotExist fileDeletePaths

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  let permDelArgList = ["-t", trashDir, "x"] <> fileDeleteNames
  runSafeRm permDelArgList

  -- file assertions
  -- Our mock FuncIO alternates returning 'n' and 'y' to getChar, so without
  -- the force option we should delete 2,4 and leave 1,3,5.
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["2", "4"]
  assertFilesExist $ mkAllTrashPaths trashDir ["1", "3", "5"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "1" "/safe-rm/functional/x/deletesNoForce/1",
          mkPathData PathTypeFile "2" "/safe-rm/functional/x/deletesNoForce/2",
          mkPathData PathTypeFile "3" "/safe-rm/functional/x/deletesNoForce/3",
          mkPathData PathTypeFile "4" "/safe-rm/functional/x/deletesNoForce/4",
          mkPathData PathTypeFile "5" "/safe-rm/functional/x/deletesNoForce/5"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 5,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "1" "/safe-rm/functional/x/deletesNoForce/1",
          mkPathData PathTypeFile "3" "/safe-rm/functional/x/deletesNoForce/3",
          mkPathData PathTypeFile "5" "/safe-rm/functional/x/deletesNoForce/5"
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesWildcards :: IO FilePath -> TestTree
deletesWildcards args = testCase "Permanently deletes several paths via wildcards" $ do
  testDir <- getTestPath args "deletesWildcards"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
      otherFiles = (testDir </>) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]
      delArgList = ("d" : filesToDelete <> otherFiles) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  createFiles (filesToDelete <> otherFiles)
  assertFilesExist filesToDelete
  assertFilesExist otherFiles

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "1f", "2f", "3f"]
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]
  assertFilesDoNotExist filesToDelete
  assertFilesDoNotExist otherFiles

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  -- leave g alone
  let permDelArgList = ["x", "*f*", "-f", "-t", trashDir]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesToDelete
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/x/deletesWildcards/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/x/deletesWildcards/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/x/deletesWildcards/f3",
          mkPathData PathTypeFile "1f" "/safe-rm/functional/x/deletesWildcards/1f",
          mkPathData PathTypeFile "2f" "/safe-rm/functional/x/deletesWildcards/2f",
          mkPathData PathTypeFile "3f" "/safe-rm/functional/x/deletesWildcards/3f",
          mkPathData PathTypeFile "g1" "/safe-rm/functional/x/deletesWildcards/g1",
          mkPathData PathTypeFile "g2" "/safe-rm/functional/x/deletesWildcards/g2",
          mkPathData PathTypeFile "g3" "/safe-rm/functional/x/deletesWildcards/g3",
          mkPathData PathTypeFile "1g" "/safe-rm/functional/x/deletesWildcards/1g",
          mkPathData PathTypeFile "2g" "/safe-rm/functional/x/deletesWildcards/2g",
          mkPathData PathTypeFile "3g" "/safe-rm/functional/x/deletesWildcards/3g"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 12,
          numFiles = 12,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "g1" "/safe-rm/functional/x/deletesWildcards/g1",
          mkPathData PathTypeFile "g2" "/safe-rm/functional/x/deletesWildcards/g2",
          mkPathData PathTypeFile "g3" "/safe-rm/functional/x/deletesWildcards/g3",
          mkPathData PathTypeFile "1g" "/safe-rm/functional/x/deletesWildcards/1g",
          mkPathData PathTypeFile "2g" "/safe-rm/functional/x/deletesWildcards/2g",
          mkPathData PathTypeFile "3g" "/safe-rm/functional/x/deletesWildcards/3g"
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesSomeWildcards :: IO FilePath -> TestTree
deletesSomeWildcards args = testCase "Deletes some paths via wildcards" $ do
  testDir <- getTestPath args "deletesSomeWildcards"
  let trashDir = testDir </> ".trash"
      files = ["foobar", "fooBadbar", "fooXbar", "g1", "g2", "g3", "1g", "2g", "3g"]
      testFiles = (testDir </>) <$> files
      delArgList = ("d" : testFiles) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  createFiles testFiles
  assertFilesExist testFiles

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir files
  assertFilesDoNotExist testFiles

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  -- NOTE: fooBadbar has been mocked in Prelude such that an attempted
  -- delete will fail. This is how this test works.
  let permDelArgList = ["x", "foo**bar", "*g*", "-f", "-t", trashDir]
  runSafeRmException @ExitCode permDelArgList

  -- file assertions
  -- 1. Everything still gone from original location
  assertFilesDoNotExist testFiles
  -- 2. Only fooBadBar should be left in trash
  assertFilesExist $ mkAllTrashPaths trashDir ["fooBadbar"]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "foobar" "/safe-rm/functional/x/deletesSomeWildcards/foobar",
          mkPathData PathTypeFile "fooBadbar" "/safe-rm/functional/x/deletesSomeWildcards/fooBadbar",
          mkPathData PathTypeFile "fooXbar" "/safe-rm/functional/x/deletesSomeWildcards/fooXbar",
          mkPathData PathTypeFile "g1" "/safe-rm/functional/x/deletesSomeWildcards/g1",
          mkPathData PathTypeFile "g2" "/safe-rm/functional/x/deletesSomeWildcards/g2",
          mkPathData PathTypeFile "g3" "/safe-rm/functional/x/deletesSomeWildcards/g3",
          mkPathData PathTypeFile "1g" "/safe-rm/functional/x/deletesSomeWildcards/1g",
          mkPathData PathTypeFile "2g" "/safe-rm/functional/x/deletesSomeWildcards/2g",
          mkPathData PathTypeFile "3g" "/safe-rm/functional/x/deletesSomeWildcards/3g"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 9,
          numFiles = 9,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "fooBadbar" "/safe-rm/functional/x/deletesSomeWildcards/fooBadbar"
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "x"
