-- | Tests for r command.
--
-- @since 0.1
module Functional.Commands.R
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Restore (r)"
    [ restoreOne args,
      restoreMany args,
      restoreUnknownError args,
      restoreCollisionError args,
      restoresSome args,
      restoresSomeNoTrace args
    ]

restoreOne :: IO FilePath -> TestTree
restoreOne args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "r1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm "LIST 1" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE

  let restoreArgList = ["r", "f1", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs "RESTORE" restoreArgList

  -- list output assertions
  restoreResult <- captureSafeRm "LIST 2" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist [f1]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]
  pure $ capturedToBs [delResult, logs, restoreResult]
  where
    desc = "Restores a single file"
    gpath = goldenPath </> "single.golden"

restoreMany :: IO FilePath -> TestTree
restoreMany args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "r2"
      trashDir = testDir </> ".trash"
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

  -- list output assertions
  delResult <- captureSafeRm "LIST 1" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete

  -- RESTORE

  let restoreArgList =
        -- do not restore f2
        ["r", "f1", "f3", "dir1", "dir2", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs "RESTORE" restoreArgList

  -- list output assertions
  restoreResult <- captureSafeRm "LIST 2" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f3"]
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]
  pure $ capturedToBs [delResult, logs, restoreResult]
  where
    desc = "Restores several paths"
    gpath = goldenPath </> "many.golden"

restoreUnknownError :: IO FilePath -> TestTree
restoreUnknownError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "r3"
      trashDir = testDir </> ".trash"
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

  -- list output assertions
  delResult <- captureSafeRm "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "bad file", "-t", trashDir]
  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ExitCode
      "RESTORE"
      restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  pure $ capturedToBs [delResult, ex, logs]
  where
    desc = "Restore unknown prints error"
    gpath = goldenPath </> "unknown.golden"

restoreCollisionError :: IO FilePath -> TestTree
restoreCollisionError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "r4"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first and recreate
  runSafeRm delArgList
  createFiles [f1]

  -- list output assertions
  delResult <- captureSafeRm "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "f1", "-t", trashDir]
  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ExitCode
      "RESTORE"
      restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  pure $ capturedToBs [delResult, ex, logs]
  where
    desc = "Restore collision prints error"
    gpath = goldenPath </> "collision.golden"

restoresSome :: IO FilePath -> TestTree
restoresSome args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "r5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = ("d" : realFiles) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm "LIST 1" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList =
        ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ExitCode
      "RESTORE"
      restoreArgList

  -- list output assertions
  resultList <- captureSafeRm "LIST 2" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])
  pure $ capturedToBs [delResult, ex, logs, resultList]
  where
    desc = "Restores some, errors on others"
    gpath = goldenPath </> "some.golden"

restoresSomeNoTrace :: IO FilePath -> TestTree
restoresSomeNoTrace args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "r5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = ("d" : realFiles) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm "LIST 1" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList =
        ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ExitCode
      "RESTORE"
      restoreArgList

  -- list output assertions
  resultList <- captureSafeRm "LIST 2" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])
  pure $ capturedToBs [delResult, ex, logs, resultList]
  where
    desc = "Restores some no trace"
    gpath = goldenPath </> "no-trace.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/R"
