-- | Tests for r command.
--
-- @since 0.1
module Functional.Commands.R
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
    "Restore (r)"
    [ restoreOne args,
      restoreMany args,
      restoreUnknownError args,
      restoreCollisionError args,
      restoresSome args
    ]

restoreOne :: IO FilePath -> TestTree
restoreOne args = testCase "Restores a single file" $ do
  testDir <- getTestPath args "restoreOne"
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

  -- RESTORE

  let restoreArgList = ["r", "f1", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist [f1]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreOne/f1"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = mempty

restoreMany :: IO FilePath -> TestTree
restoreMany args = testCase "Restores several paths" $ do
  testDir <- getTestPath args "restoreMany"
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

  assertDirectoriesExist ((testDir </>) <$> ["dir1", "dir2/dir3"])
  assertFilesExist ((testDir </> "dir2/dir3/foo") : filesToDelete)

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir

  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList =
        -- do not restore f2
        ["r", "f1", "f3", "dir1", "dir2", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f3"]
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreMany/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoreMany/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/r/restoreMany/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/r/restoreMany/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/r/restoreMany/dir2"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.singleton (mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoreMany/f2")
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoreUnknownError :: IO FilePath -> TestTree
restoreUnknownError args = testCase "Restore unknown prints error" $ do
  testDir <- getTestPath args "restoreUnknownError"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a restore, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the restore
  -- failure only.
  clearDirectory testDir
  createFiles [f1]

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

  -- RESTORE
  let restoreArgList = ["r", "bad file", "-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet restoreIdxSet
  delExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreUnknownError/f1"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoreCollisionError :: IO FilePath -> TestTree
restoreCollisionError args = testCase "Restore collision prints error" $ do
  testDir <- getTestPath args "restoreCollisionError"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first and recreate
  runSafeRm delArgList
  createFiles [f1]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = ["r", "f1", "-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet restoreIdxSet
  delExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreCollisionError/f1"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresSome :: IO FilePath -> TestTree
restoresSome args = testCase "Restores some, errors on others" $ do
  testDir <- getTestPath args "restoresSome"
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
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

  -- RESTORE
  let restoreArgList = ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoresSome/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoresSome/f2",
          mkPathData PathTypeFile "f5" "/safe-rm/functional/r/restoresSome/f5"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = mempty

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot d = do
  root <- mroot
  let rdir = root </> "r"
  createDirectoryIfMissing False rdir
  pure $ rdir </> d
