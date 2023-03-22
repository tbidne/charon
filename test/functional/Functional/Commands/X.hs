-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Permanent Delete (x)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deletesSome args,
      deletesSomeNoTrace args,
      deletesNoForce args
    ]

deletesOne :: IO FilePath -> TestTree
deletesOne args = testCase "Permanently deletes a single file" $ do
  tmpDir <- args
  let testDir = tmpDir </> "x1"
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

  -- PERMANENT DELETE

  let permDelArgList = ["x", "f1", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs permDelArgList

  -- list output assertions
  permDelResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ f1 : mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  assertMatches expectedTerminal1 delResult
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 permDelResult
  where
    expectedTerminal1 =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/x1/f1",
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
      [ Outfix "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/x1/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f1",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x1/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
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

deletesMany :: IO FilePath -> TestTree
deletesMany args = testCase "Permanently deletes several paths" $ do
  tmpDir <- args
  let testDir = tmpDir </> "x2"
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
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- PERMANENT DELETE

  let permDelArgList =
        -- leave f2 alone
        ["x", "f1", "f3", "dir1", "dir2", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs permDelArgList

  -- list output assertions
  permDelResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesToDelete
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2/dir3"]
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]

  assertMatches expectedTerminal1 delResult
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 permDelResult
  where
    expectedTerminal1 =
      [ Exact "Type:     Directory",
        Exact "Name:     dir1",
        Outfix "Original: " "/safe-rm/functional/x2/dir1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     Directory",
        Exact "Name:     dir2",
        Outfix "Original: " "/safe-rm/functional/x2/dir2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/x2/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/x2/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f3",
        Outfix "Original: " "/safe-rm/functional/x2/f3",
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
      [ Outfix "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/x2/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f1",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x2/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f3",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x2/f3\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: dir1",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x2/dir1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: dir2",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x2/dir2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedTerminal2 =
      [ Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/x2/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

deleteUnknownError :: IO FilePath -> TestTree
deleteUnknownError args = testCase "Delete unknown prints error" $ do
  tmpDir <- args
  let testDir = tmpDir </> "x3"
      trashDir = testDir </> ".trash"
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

  -- list output assertions
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ["x", "bad file", "-f", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- assert exception
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  assertMatches expectedTerminal delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/x3/f1",
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
      [ Outfix "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/x3/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: bad file",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Warn][src/SafeRm.hs] No entry for 'bad file'; did not find '" "/safe-rm/functional/x3/.trash/info/bad file.json'",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]

deletesSome :: IO FilePath -> TestTree
deletesSome args = testCase "Deletes some, errors on others" $ do
  tmpDir <- args
  let testDir = tmpDir </> "x4"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]
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

  -- PERMANENT DELETE
  let permDelArgList =
        ("x" : filesTryPermDelete) <> ["-f", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- list output assertions
  resultList <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesTryPermDelete

  assertMatches expectedTerminal1 delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 resultList
  where
    expectedTerminal1 =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/x4/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/x4/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f5",
        Outfix "Original: " "/safe-rm/functional/x4/f5",
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
      [ Outfix "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/x4/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f1",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x4/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f2",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x4/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f3",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Warn][src/SafeRm.hs] No entry for 'f3'; did not find '" "/safe-rm/functional/x4/.trash/info/f3.json'",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f4",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Warn][src/SafeRm.hs] No entry for 'f4'; did not find '" "/safe-rm/functional/x4/.trash/info/f4.json'",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f5",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x4/f5\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
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

deletesSomeNoTrace :: IO FilePath -> TestTree
deletesSomeNoTrace args = testCase "Deletes some no trace" $ do
  tmpDir <- args
  let testDir = tmpDir </> "x5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]
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

  -- PERMANENT DELETE
  let permDelArgList =
        ("x" : filesTryPermDelete) <> ["-f", "-t", trashDir]
  (ex, logs) <-
    captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- list output assertions
  resultList <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesTryPermDelete

  assertMatches expectedTerminal1 delResult
  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 resultList
  where
    expectedTerminal1 =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/x5/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/x5/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f5",
        Outfix "Original: " "/safe-rm/functional/x5/f5",
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
      [ Outfix "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/x5/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f1",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x5/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f2",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x5/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f3",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Warn][src/SafeRm.hs] No entry for 'f3'; did not find '" "/safe-rm/functional/x5/.trash/info/f3.json'",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f4",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Warn][src/SafeRm.hs] No entry for 'f4'; did not find '" "/safe-rm/functional/x5/.trash/info/f4.json'",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: f5",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x5/f5\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
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

deletesNoForce :: IO FilePath -> TestTree
deletesNoForce args = testCase "Permanently deletes several paths without --force" $ do
  tmpDir <- args
  let testDir = tmpDir </> "x6"
      trashDir = testDir </> ".trash"
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

  -- PERMANENT DELETE

  let permDelArgList = ["-t", trashDir, "x"] <> fileDeleteNames
  (permDelResult, logs) <- captureSafeRmLogs permDelArgList

  -- list output assertions
  listResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  -- Our mock FuncIO alternates returning 'n' and 'y' to getChar, so without
  -- the force option we should delete 2,4 and leave 1,3,5.
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["2", "4"]
  assertFilesExist $ mkAllTrashPaths trashDir ["1", "3", "5"]
  assertDirectoriesExist [trashDir]

  assertMatches expectedTerminal1 permDelResult
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 listResult
  where
    expectedTerminal1 =
      [ Exact "",
        Exact "Type:      File",
        Exact "Name:      1",
        Outfix "Original:  " "/safe-rm/functional/x6/1",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        Exact "Permanently delete (y/n)?",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      2",
        Outfix "Original:  " "/safe-rm/functional/x6/2",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        Exact "Permanently delete (y/n)?",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      3",
        Outfix "Original:  " "/safe-rm/functional/x6/3",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        Exact "Permanently delete (y/n)?",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      4",
        Outfix "Original:  " "/safe-rm/functional/x6/4",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        Exact "Permanently delete (y/n)?",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      5",
        Outfix "Original:  " "/safe-rm/functional/x6/5",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        Exact "Permanently delete (y/n)?"
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/x6/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: 1",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: 2",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x6/2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: 3",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: 4",
        Outfix "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleted: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"4\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/x6/4\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting.deleteTrashPath][Debug][src/SafeRm/Trash.hs] Deleting: 5"
      ]

    expectedTerminal2 =
      [ Exact "Type:     File",
        Exact "Name:     1",
        Outfix "Original: " "/safe-rm/functional/x6/1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     3",
        Outfix "Original: " "/safe-rm/functional/x6/3",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     5",
        Outfix "Original: " "/safe-rm/functional/x6/5",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]
