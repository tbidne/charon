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
restoreOne args = testCase "Restores a single file" $ do
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE

  let restoreArgList = ["r", "f1", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs restoreArgList

  -- list output assertions
  restoreResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist [f1]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]
  -- pure $ capturedToBs [delResult, logs, restoreResult]

  assertMatches expectedTerminal1 delResult
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 restoreResult
  where
    expectedTerminal1 =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/r1/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/r1/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f1",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r1/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedTerminal2 =
      Exact
        <$> [ "",
              "",
              "Entries:      0",
              "Total Files:  0",
              "Log size:     0.00B",
              "Size:         0.00B",
              ""
            ]

restoreMany :: IO FilePath -> TestTree
restoreMany args = testCase "Restores several paths" $ do
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete

  -- RESTORE

  let restoreArgList =
        -- do not restore f2
        ["r", "f1", "f3", "dir1", "dir2", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs restoreArgList

  -- list output assertions
  restoreResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f3"]
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]

  assertMatches expectedTerminal1 delResult
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 restoreResult
  where
    expectedTerminal1 =
      [ Exact "Type:     Directory",
        Exact "Name:     dir1",
        Outfix "Original: " "/safe-rm/functional/r2/dir1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     Directory",
        Exact "Name:     dir2",
        Outfix "Original: " "/safe-rm/functional/r2/dir2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/r2/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/r2/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f3",
        Outfix "Original: " "/safe-rm/functional/r2/f3",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/r2/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f1",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r2/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f3",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r2/f3\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: dir1",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r2/dir1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: dir2",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r2/dir2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedTerminal2 =
      [ Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/r2/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

restoreUnknownError :: IO FilePath -> TestTree
restoreUnknownError args = testCase "Restore unknown prints error" $ do
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "bad file", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  assertMatches expectedTerminal delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/r3/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/r3/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: bad file",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs] No entry for 'bad file'; did not find '" "/safe-rm/functional/r3/.trash/info/bad file.json'",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]

restoreCollisionError :: IO FilePath -> TestTree
restoreCollisionError args = testCase "Restore collision prints error" $ do
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "f1", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  assertMatches expectedTerminal delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/r4/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/r4/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f1",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs] Cannot restore the trash file 'f1' as one exists at the original location: " "/safe-rm/functional/r4/f1",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]

restoresSome :: IO FilePath -> TestTree
restoresSome args = testCase "Restores some, errors on others" $ do
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList =
        ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  -- list output assertions
  resultList <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])

  assertMatches expectedTerminal1 delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 resultList
  where
    expectedTerminal1 =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/r5/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/r5/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f5",
        Outfix "Original: " "/safe-rm/functional/r5/f5",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/r5/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f1",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r5/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f2",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r5/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f3",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs] No entry for 'f3'; did not find '" "/safe-rm/functional/r5/.trash/info/f3.json'",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f4",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs] No entry for 'f4'; did not find '" "/safe-rm/functional/r5/.trash/info/f4.json'",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f5",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r5/f5\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]
    expectedTerminal2 =
      [ Exact "",
        Exact "",
        Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

restoresSomeNoTrace :: IO FilePath -> TestTree
restoresSomeNoTrace args = testCase "Restores some no trace" $ do
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList =
        ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  -- list output assertions
  resultList <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])

  assertMatches expectedTerminal1 delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 resultList
  where
    expectedTerminal1 =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/r5/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/r5/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f5",
        Outfix "Original: " "/safe-rm/functional/r5/f5",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/r5/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f1",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r5/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f2",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r5/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f3",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs] No entry for 'f3'; did not find '" "/safe-rm/functional/r5/.trash/info/f3.json'",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f4",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs] No entry for 'f4'; did not find '" "/safe-rm/functional/r5/.trash/info/f4.json'",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restoring: f5",
        Outfix "[2020-05-31 12:00:00][functional.restore.restoring.mvTrashToOriginal][Debug][src/SafeRm/Trash.hs] Restored: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/r5/f5\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]

    expectedTerminal2 =
      [ Exact "",
        Exact "",
        Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]
