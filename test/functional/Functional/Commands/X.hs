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
      deletesNoForce args
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

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot d = do
  root <- mroot
  let rdir = root </> "x"
  createDirectoryIfMissing False rdir
  pure $ rdir </> d
