-- | Tests for d command.
--
-- @since 0.1
module Functional.Commands.D
  ( tests,
  )
where

import Functional.Prelude

-- TODO: It would be nice if we could verify that the original location
-- is correct. Recently a bug was fixed as directories were using relative
-- paths. Evidently the tests did not catch this, presumably because
-- relative paths are sufficient here.

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Delete (d)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deleteDuplicateFile args,
      deletesSome args,
      deletesNoTrace args
    ]

deletesOne :: IO FilePath -> TestTree
deletesOne args = testCase "Deletes a single file" $ do
  tmpDir <- args
  let testDir = tmpDir </> "d1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = ["d", f1, "-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  (_, logs) <- captureSafeRmLogs argList

  -- list output assertions
  result <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  assertMatches expectedLogs logs
  assertMatches expectedTerminal result
  where
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs] Trash home:" "safe-rm/functional/d1/.trash",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI =" "/safe-rm/functional/d1/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d1/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedTerminal =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/d1/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

deletesMany :: IO FilePath -> TestTree
deletesMany args = testCase "Deletes many paths" $ do
  tmpDir <- args
  let testDir = tmpDir </> "d2"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      argList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  (_, logs) <- captureSafeRmLogs argList

  -- list output assertions
  result <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]

  assertMatches expectedLogs logs
  assertMatches expectedTerminal result
  where
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/d2/.trash",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/f3\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/f3\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/dir1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/dir1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/dir2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d2/dir2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedTerminal =
      [ Exact "Type:     Directory",
        Exact "Name:     dir1",
        Outfix "Original: " "/safe-rm/functional/d2/dir1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     Directory",
        Exact "Name:     dir2",
        Outfix "Original: " "/safe-rm/functional/d2/dir2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/d2/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/d2/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f3",
        Outfix "Original: " "/safe-rm/functional/d2/f3",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

deleteUnknownError :: IO FilePath -> TestTree
deleteUnknownError args = testCase "Deletes unknown prints error" $ do
  tmpDir <- args
  let testDir = tmpDir </> "d3"
      trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  (ex, logs) <- captureSafeRmExceptionLogs @ExitCode argList

  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  where
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/d3/.trash",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting][Warn][src/SafeRm.hs] Path not found: " "/safe-rm/functional/d3/bad file",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]

deleteDuplicateFile :: IO FilePath -> TestTree
deleteDuplicateFile args = testCase "Deletes duplicate file" $ do
  tmpDir <- args
  let testDir = tmpDir </> "d4"
      trashDir = testDir </> ".trash"
      file = testDir </> "f1"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- create and delete twice
  createFiles [file]
  assertFilesExist [file]
  (_, logs1) <- captureSafeRmLogs argList

  createFiles [file]
  assertFilesExist [file]
  (_, logs2) <- captureSafeRmLogs argList

  result <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1 (1)", "f1"]
  assertFilesDoNotExist [file]
  assertDirectoriesExist [trashDir]

  assertMatches expectedLogs1 logs1
  assertMatches expectedLogs2 logs2
  assertMatches expectedTerminal result
  where
    expectedLogs1 =
      [ Outfix "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/d4/.trash",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d4/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d4/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedLogs2 =
      [ Outfix "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/d4/.trash",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1 (1)\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d4/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1 (1)\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d4/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}"
      ]

    expectedTerminal =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/d4/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f1 (1)",
        Outfix "Original: " "/safe-rm/functional/d4/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      2",
        Exact "Total Files:  2",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

deletesSome :: IO FilePath -> TestTree
deletesSome args = testCase "Deletes some files with errors" $ do
  tmpDir <- args
  let testDir = tmpDir </> "d5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryDelete = (testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]
      argList = ("d" : filesTryDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  (ex, logs) <-
    captureSafeRmExceptionLogs @ExitCode argList

  -- list output assertions
  resultList <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist $ mkTrashPaths trashDir ["f3", "f4"]

  "ExitFailure 1" @=? ex
  assertMatches expectedLogs logs
  assertMatches expectedTerminal resultList
  where
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/d5/.trash",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d5/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d5/f1\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d5/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d5/f2\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting][Warn][src/SafeRm.hs] Path not found: " "/safe-rm/functional/d5/f3",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting][Warn][src/SafeRm.hs] Path not found: " "/safe-rm/functional/d5/f4",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Deleting: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d5/f5\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Outfix "[2020-05-31 12:00:00][functional.delete.deleting.mvOriginalToTrash][Debug][src/SafeRm/Trash.hs] Moved to trash: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = " "/safe-rm/functional/d5/f5\"}, size = MkBytes 5, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] ExitFailure 1"
      ]

    expectedTerminal =
      [ Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/d5/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original:" "/safe-rm/functional/d5/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f5",
        Outfix "Original: " "/safe-rm/functional/d5/f5",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

deletesNoTrace :: IO FilePath -> TestTree
deletesNoTrace args = testCase "Delete failures without trace" $ do
  tmpDir <- args
  let testDir = tmpDir </> "d6"
      trashDir = testDir </> ".trash"
      toDeleteNames = ["f1", "f3", "f5"]
      toDelete = fmap (testDir </>) toDeleteNames
      argList =
        ["-t", trashDir]
          <> ("d" : ((testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]))

  -- setup
  clearDirectory testDir
  createFiles toDelete
  assertFilesExist toDelete

  (ex, _) <- captureSafeRmExceptionLogs @ExitCode argList

  -- file assertions
  assertFilesExist $ mkTrashPaths trashDir toDeleteNames
  assertFilesDoNotExist toDelete

  "ExitFailure 1" @=? ex
