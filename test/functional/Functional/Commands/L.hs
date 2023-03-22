-- | Tests for l command.
--
-- @since 0.1
module Functional.Commands.L
  ( tests,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Functional.Prelude
import SafeRm.Exception
  ( TrashInfoDirNotFoundE,
    TrashInfoNotFoundE,
    TrashPathDirNotFoundE,
    TrashPathNotFoundE,
  )

-- import Data.ByteString qualified as BS

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "List (l)"
    [ emptySucceeds args,
      noPathsError args,
      noInfoError args,
      missingPathError args,
      missingInfoError args
    ]

emptySucceeds :: IO FilePath -> TestTree
emptySucceeds args = testCase "List on empty directory succeeds" $ do
  tmpDir <- args
  let argList = ["-t", tmpDir </> "l1/.trash", "l", "--format", "m"]

  (result, logs) <- captureSafeRmLogs argList

  assertMatches expectedTerminal result
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      [ Exact "",
        Exact "",
        Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Log size:     0.00B",
        Exact "Size:         0.00B",
        Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l1/.trash",
        Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash does not exist.",
        Outfix "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l1/.trash",
        Exact "[2020-05-31 12:00:00][functional.getMetadata][Info][src/SafeRm.hs] Trash does not exist."
      ]

noPathsError :: IO FilePath -> TestTree
noPathsError args = testCase "No Paths Error" $ do
  tmpDir <- args
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "info")

  (ex, logs) <- captureSafeRmExceptionLogs @TrashPathDirNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix "The trash paths directory was not found at '" "/safe-rm/functional/l2/.trash/paths' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l2/.trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash paths directory was not found at '" "/safe-rm/functional/l2/.trash/paths' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e)."
      ]

noInfoError :: IO FilePath -> TestTree
noInfoError args = testCase "No Info Error" $ do
  tmpDir <- args
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "paths")

  (ex, logs) <- captureSafeRmExceptionLogs @TrashInfoDirNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix "The trash info directory was not found at '" "/safe-rm/functional/l2/.trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l2/.trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash info directory was not found at '" "/safe-rm/functional/l2/.trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e)."
      ]

missingPathError :: IO FilePath -> TestTree
missingPathError args = testCase "Entry Missing Path" $ do
  tmpDir <- args
  let testDir = tmpDir </> "l3"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]
      missingInfo =
        Char8.unlines
          [ "{",
            "\"created\":",
            "\"2020-05-31 12:00:00\",",
            "\"original\":",
            Char8.pack ("\"" <> testDir </> "missing\","),
            "\"size\":",
            "5,",
            "\"type\":",
            "\"f\"",
            "}"
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "paths")
  clearDirectory (trashDir </> "info")
  createFileContents [(trashDir </> "info" </> "missing.json", missingInfo)]

  -- Creating empty file so that we don't get the "size mismatch" error.
  -- We specifically want the "missing.json has no corresponding missing" error.
  createFiles [trashDir </> "paths" </> "blah"]

  (ex, logs) <- captureSafeRmExceptionLogs @TrashPathNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The path 'missing' was not found in '"
        ["/safe-rm/functional/l3/.trash/paths' despite being listed in '"]
        "/safe-rm/functional/l3/.trash/info'. This can be fixed by manually deleting the .json file or deleting everything (i.e. sr e)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l3/.trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/l3/.trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: [\"missing.json\"]",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: " "/safe-rm/functional/l3/.trash/info/missing.json",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The path 'missing' was not found in '"
          ["/safe-rm/functional/l3/.trash/paths' despite being listed in '"]
          "/safe-rm/functional/l3/.trash/info'. This can be fixed by manually deleting the .json file or deleting everything (i.e. sr e)."
      ]

missingInfoError :: IO FilePath -> TestTree
missingInfoError args = testCase "Entry Missing Info" $ do
  tmpDir <- args
  let testDir = tmpDir </> "l5"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "paths")
  clearDirectory (trashDir </> "info")
  createFiles [trashDir </> "paths" </> "bar"]

  (ex, logs) <- captureSafeRmExceptionLogs @TrashInfoNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The path 'bar.json' was not found in '"
        ["/safe-rm/functional/l5/.trash/info' despite being listed in '"]
        "/safe-rm/functional/l5/.trash/paths'. This can be fixed by manually deleting the /paths entry or deleting everything (i.e. sr e)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l5/.trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/l5/.trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: []",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: [\"bar\"]",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The path 'bar.json' was not found in '"
          ["/safe-rm/functional/l5/.trash/info' despite being listed in '"]
          "/safe-rm/functional/l5/.trash/paths'. This can be fixed by manually deleting the /paths entry or deleting everything (i.e. sr e)."
      ]
