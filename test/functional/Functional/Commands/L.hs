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
  ( TrashDirFilesNotFoundE,
    TrashDirInfoNotFoundE,
    TrashEntryFileNotFoundE,
    TrashEntryInfoNotFoundE,
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
  testDir <- getTestPath args "emptySucceeds"
  let argList = ["-t", testDir </> ".trash", "l", "--format", "m"]

  (result, logs) <- captureSafeRmLogs argList

  assertMatches expectedTerminal result
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      [ Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l/emptySucceeds/.trash",
        Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash does not exist."
      ]

noPathsError :: IO FilePath -> TestTree
noPathsError args = testCase "No Paths Error" $ do
  testDir <- getTestPath args "noPathsError"
  let trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "info")

  (ex, logs) <- captureSafeRmExceptionLogs @TrashDirFilesNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix "The trash files directory was not found at '" "/safe-rm/functional/l/noPathsError/.trash/files' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l/noPathsError/.trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash files directory was not found at '" "/safe-rm/functional/l/noPathsError/.trash/files' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e -f)."
      ]

noInfoError :: IO FilePath -> TestTree
noInfoError args = testCase "No Info Error" $ do
  testDir <- getTestPath args "noInfoError"
  let trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "files")

  (ex, logs) <- captureSafeRmExceptionLogs @TrashDirInfoNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix "The trash info directory was not found at '" "/safe-rm/functional/l/noInfoError/.trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l/noInfoError/.trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash info directory was not found at '" "/safe-rm/functional/l/noInfoError/.trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. sr e -f)."
      ]

missingPathError :: IO FilePath -> TestTree
missingPathError args = testCase "Entry Missing Path" $ do
  testDir <- getTestPath args "missingPathError"
  let trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]
      missingInfo =
        Char8.unlines
          [ "{",
            "\"created\":",
            "\"2020-05-31 12:00:00\",",
            "\"original\":",
            Char8.pack (escapeBackslashes $ "\"" <> testDir </> "missing\","),
            "\"size\":",
            "5,",
            "\"type\":",
            "\"f\"",
            "}"
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "files")
  clearDirectory (trashDir </> "info")
  createFileContents [(trashDir </> "info" </> "missing.json", missingInfo)]

  -- Creating empty file so that we don't get the "size mismatch" error.
  -- We specifically want the "missing.json has no corresponding missing" error.
  createFiles [trashDir </> "files" </> "blah"]

  (ex, logs) <- captureSafeRmExceptionLogs @TrashEntryFileNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The file 'missing' was not found in '"
        ["/safe-rm/functional/l/missingPathError/.trash/files' despite being listed in '"]
        "/safe-rm/functional/l/missingPathError/.trash/info'. This can be fixed by manually deleting the .json file or deleting everything (i.e. sr e -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l/missingPathError/.trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/l/missingPathError/.trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: [\"missing.json\"]",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: " "/safe-rm/functional/l/missingPathError/.trash/info/missing.json",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The file 'missing' was not found in '"
          ["/safe-rm/functional/l/missingPathError/.trash/files' despite being listed in '"]
          "/safe-rm/functional/l/missingPathError/.trash/info'. This can be fixed by manually deleting the .json file or deleting everything (i.e. sr e -f)."
      ]

    -- Windows paths have backslashes which isn't valid json. Need to escape
    escapeBackslashes [] = []
    escapeBackslashes ('\\' : xs) = '\\' : '\\' : escapeBackslashes xs
    escapeBackslashes (x : xs) = x : escapeBackslashes xs

missingInfoError :: IO FilePath -> TestTree
missingInfoError args = testCase "Entry Missing Info" $ do
  testDir <- getTestPath args "missingInfoError"
  let trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "files")
  clearDirectory (trashDir </> "info")
  createFiles [trashDir </> "files" </> "bar"]

  (ex, logs) <- captureSafeRmExceptionLogs @TrashEntryInfoNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The file 'bar.json' was not found in '"
        ["/safe-rm/functional/l/missingInfoError/.trash/info' despite being listed in '"]
        "/safe-rm/functional/l/missingInfoError/.trash/files'. This can be fixed by manually deleting the /files entry or deleting everything (i.e. sr e -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " "/safe-rm/functional/l/missingInfoError/.trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " "/safe-rm/functional/l/missingInfoError/.trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: []",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: [\"bar\"]",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The file 'bar.json' was not found in '"
          ["/safe-rm/functional/l/missingInfoError/.trash/info' despite being listed in '"]
          "/safe-rm/functional/l/missingInfoError/.trash/files'. This can be fixed by manually deleting the /files entry or deleting everything (i.e. sr e -f)."
      ]

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "l"
