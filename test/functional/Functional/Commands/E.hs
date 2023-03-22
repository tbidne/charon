-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.E
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Empty (e)"
    [ emptyTrash args,
      emptyTrashTwice args,
      emptyNoForce args,
      missingInfoForcesDelete args,
      missingPathsForcesDelete args
    ]

emptyTrash :: IO FilePath -> TestTree
emptyTrash args = testCase "Empties trash" $ do
  tmpDir <- args
  let testDir = tmpDir </> "e1"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- list output assertions
  resultDel <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesExist $ mkTrashInfoPaths trashDir ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- EMPTY

  let emptyArgList = ["e", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs emptyArgList

  -- list output assertions
  result <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertMatches expectedTerminal1 resultDel
  assertMatches expectedLogs logs
  assertMatches expectedTerminal2 result
  where
    expectedTerminal1 =
      [ Exact "Type:     Directory",
        Exact "Name:     dir1",
        Outfix "Original: " "/safe-rm/functional/e1/dir1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     Directory",
        Exact "Name:     dir2",
        Outfix "Original: " "/safe-rm/functional/e1/dir2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/e1/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/e1/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f3",
        Outfix "Original: " "/safe-rm/functional/e1/f3",
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
      [ Outfix "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/e1/.trash"
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

emptyTrashTwice :: IO FilePath -> TestTree
emptyTrashTwice args = testCase "Calling empty twice does not error" $ do
  tmpDir <- args
  let testDir = tmpDir </> "e2"
      trashDir = testDir </> ".trash"

  (_, logs1) <- captureSafeRmLogs ["e", "-f", "-t", trashDir]

  (_, logs2) <- captureSafeRmLogs ["e", "-f", "-t", trashDir]

  assertMatches expectedLogs logs1
  assertMatches expectedLogs logs2
  where
    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/e2/.trash",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Trash home does not exist."
      ]

emptyNoForce :: IO FilePath -> TestTree
emptyNoForce args = testCase "Empties trash without force" $ do
  tmpDir <- args
  let testDir = tmpDir </> "e3"
      trashDir = testDir </> ".trash"
      fileDeleteNames = show @Int <$> [1 .. 5]
      fileDeletePaths = (testDir </>) <$> fileDeleteNames
      delArgList = ["-t", trashDir, "d"] <> fileDeletePaths

  -- setup
  clearDirectory testDir
  -- test w/ a file in dir
  createFiles fileDeletePaths
  assertFilesExist fileDeletePaths

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames
  assertFilesDoNotExist fileDeletePaths

  -- EMPTY

  let emptyArgList = ["-t", trashDir, "e"]
  (emptyResult, emptyLogs) <- captureSafeRmLogs emptyArgList

  -- list output assertions
  listResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  -- First getChar response was 'n', so files should still exist
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames

  assertMatches expectedTerminal1 emptyResult
  assertMatches expectedLogs emptyLogs
  assertMatches expectedTerminal2 listResult
  where
    expectedTerminal1 =
      [ Exact "",
        Exact "Entries:      5",
        Exact "Total Files:  5",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact "",
        Exact "Permanently delete all contents (y/n)?"
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/e3/.trash",
        Outfix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/e3/.trash",
        Outfix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/e3/.trash/info",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: [",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: ",
        Prefix "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Index size: 5",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Num entries: 5",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Log does not exist",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Num all files: 5",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Total size: MkSomeSize SB (MkBytes 0.0)",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Not deleting contents."
      ]

    expectedTerminal2 =
      [ Exact "Type:     File",
        Exact "Name:     1",
        Outfix "Original: " "/safe-rm/functional/e3/1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     2",
        Outfix "Original: " "/safe-rm/functional/e3/2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     3",
        Outfix "Original: " "/safe-rm/functional/e3/3",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     4",
        Outfix "Original: " "/safe-rm/functional/e3/4",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     5",
        Outfix "Original: " "/safe-rm/functional/e3/5",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      5",
        Exact "Total Files:  5",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

missingInfoForcesDelete :: IO FilePath -> TestTree
missingInfoForcesDelete args = testCase "empty --force overwrites bad directory (no info.)" $ do
  tmpDir <- args
  let testDir = tmpDir </> "e4"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  -- delete files
  runSafeRm delArgList

  -- file assertions
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- delete info dir, leaving trash dir in bad state
  clearDirectory (trashDir </> "info")

  let emptyArgList = ["-t", trashDir, "e", "-f"]
  (emptyResult, emptyLogs) <- captureSafeRmLogs emptyArgList

  metadataResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertDirectoriesExist $ fmap (trashDir </>) ["info", "paths"]

  assertMatches [] emptyResult
  assertMatches expectedEmptyLogs emptyLogs
  assertMatches expectedMetadata metadataResult
  where
    expectedEmptyLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Trash home:" "/safe-rm/functional/e4/.trash"
      ]

    expectedMetadata =
      [ Exact "",
        Exact "",
        Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

missingPathsForcesDelete :: IO FilePath -> TestTree
missingPathsForcesDelete args = testCase "empty --force overwrites bad directory (no paths/)" $ do
  tmpDir <- args
  let testDir = tmpDir </> "e5"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  -- delete files
  runSafeRm delArgList

  -- file assertions
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- delete info dir, leaving trash dir in bad state
  clearDirectory (trashDir </> "paths")

  let emptyArgList = ["-t", trashDir, "e", "-f"]
  (emptyResult, emptyLogs) <- captureSafeRmLogs emptyArgList

  metadataResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertDirectoriesExist $ fmap (trashDir </>) ["info", "paths"]

  assertMatches [] emptyResult
  assertMatches expectedEmptyLogs emptyLogs
  assertMatches expectedMetadata metadataResult
  where
    expectedEmptyLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs] Trash home:" "/safe-rm/functional/e5/.trash"
      ]

    expectedMetadata =
      [ Exact "",
        Exact "",
        Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]
