-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.M
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Metadata (m)"
    [ metadata args,
      empty args
    ]

metadata :: IO FilePath -> TestTree
metadata args = testCase "Prints metadata" $ do
  tmpDir <- args
  let testDir = tmpDir </> "m1"
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
  delResult <- captureSafeRm ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- METADATA

  let metaArgList = ["m", "-t", trashDir]
  (metadataResult, logs) <- captureSafeRmLogs metaArgList

  -- assert nothing changed
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- pure $ capturedToBs [delResult, metadataResult, logs]
  assertMatches expectedTerminal1 delResult
  assertMatches expectedMetadata metadataResult
  assertMatches expectedLogs logs
  where
    expectedTerminal1 =
      [ Exact "Type:     Directory",
        Exact "Name:     dir1",
        Outfix "Original: " "/safe-rm/functional/m1/dir1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     Directory",
        Exact "Name:     dir2",
        Outfix "Original: " "/safe-rm/functional/m1/dir2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f1",
        Outfix "Original: " "/safe-rm/functional/m1/f1",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f2",
        Outfix "Original: " "/safe-rm/functional/m1/f2",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Type:     File",
        Exact "Name:     f3",
        Outfix "Original: " "/safe-rm/functional/m1/f3",
        Exact "Size:     5.00B",
        Exact "Created:  2020-05-31 12:00:00",
        Exact "",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

    expectedMetadata =
      Exact
        <$> [ "Entries:      5",
              "Total Files:  4",
              "Log size:     0.00B",
              "Size:         0.00B",
              ""
            ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/m1/.trash",
        Outfix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/m1/.trash/info",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: [",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: ",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: [",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Index size: 5",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Num entries: 5",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Log does not exist",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Num all files: 4",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Total size: MkSomeSize SB (MkBytes 0.0)"
      ]

empty :: IO FilePath -> TestTree
empty args = testCase "Prints empty metadata" $ do
  tmpDir <- args
  let testDir = tmpDir </> "m2"
      trashDir = testDir </> ".trash"

  createDirectories [testDir, trashDir, trashDir </> "info", trashDir </> "paths"]
  createFiles [trashDir </> "log"]

  let metaArgList = ["m", "-t", trashDir]
  (result, logs) <- captureSafeRmLogs metaArgList

  assertMatches expectedTerminal result
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      Exact
        <$> [ "Entries:      0",
              "Total Files:  0",
              "Log size:     5.00B",
              "Size:         0.00B",
              ""
            ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/m2/.trash",
        Outfix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/m2/.trash/info",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: [",
        Prefix "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: [",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Index size: 0",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Num entries: 0",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Num all files: 0",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs] Total size: MkSomeSize SB (MkBytes 0.0)"
      ]
