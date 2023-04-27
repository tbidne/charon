-- | Tests for l command.
module Functional.Commands.List
  ( tests,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as T
import Functional.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Exception
  ( TrashDirFilesNotFoundE,
    TrashDirInfoNotFoundE,
    TrashEntryFileNotFoundE,
    TrashEntryInfoNotFoundE,
  )

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "List Command"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    [ emptySucceeds backend args,
      noPathsError backend args,
      noInfoError backend args,
      missingPathError backend args,
      missingInfoError backend args
    ]

emptySucceeds :: Backend -> IO FilePath -> TestTree
emptySucceeds backend args = testCase "List on empty directory succeeds" $ do
  testDir <- getTestPath args (withBackendDir backend "emptySucceeds")
  let trashDir = testDir </> ".trash"
      argList = withSrArgs trashDir backend ["list", "--format", "m"]

  (result, logs) <- captureSafeRmLogs argList

  assertMatches expectedTerminal result
  assertMatches expectedLogs logs
  where
    expectedTerminal =
      [ Exact ""
      ]

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash does not exist."
      ]

noPathsError :: Backend -> IO FilePath -> TestTree
noPathsError backend args = testCase "No Paths Error" $ do
  testDir <- getTestPath args (withBackendDir backend "noPathsError")
  let trashDir = testDir </> ".trash"
      argList = withSrArgs trashDir backend ["list", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "info")

  (ex, logs) <- captureSafeRmExceptionLogs @TrashDirFilesNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix "The trash files directory was not found at '" ".trash/files' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash files directory was not found at '" ".trash/files' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]

noInfoError :: Backend -> IO FilePath -> TestTree
noInfoError backend args = testCase "No Info Error" $ do
  testDir <- getTestPath args (withBackendDir backend "noInfoError")
  let trashDir = testDir </> ".trash"
      argList = withSrArgs trashDir backend ["list", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "files")

  (ex, logs) <- captureSafeRmExceptionLogs @TrashDirInfoNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix "The trash info directory was not found at '" ".trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash info directory was not found at '" ".trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]

missingPathError :: Backend -> IO FilePath -> TestTree
missingPathError backend args = testCase "Entry Missing Path" $ do
  testDir <- getTestPath args (withBackendDir backend "missingPathError")
  let trashDir = testDir </> ".trash"
      argList = withSrArgs trashDir backend ["list", "--format", "m"]
      missingInfo =
        case backend of
          BackendDefault ->
            Char8.unlines
              [ "[Trash Info]",
                "Path=" <> encodeUtf8 (T.pack $ escapeBackslashes $ testDir </> "missing"),
                "DeletionDate=2020-05-31T12:00:00",
                "Size=5",
                "Type=f"
              ]
          BackendFdo ->
            Char8.unlines
              [ "[Trash Info]",
                "Path=" <> encodeUtf8 (T.pack $ escapeBackslashes $ testDir </> "missing"),
                "DeletionDate=2020-05-31T12:00:00"
              ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "files")
  clearDirectory (trashDir </> "info")
  createFileContents [(trashDir </> "info" </> "missing.trashinfo", missingInfo)]

  -- Creating empty file so that we don't get the "size mismatch" error.
  -- We specifically want the "missing.trashinfo has no corresponding missing" error.
  createFiles [trashDir </> "files" </> "blah"]

  (ex, logs) <- captureSafeRmExceptionLogs @TrashEntryFileNotFoundE argList

  assertMatch expectedErr ex
  assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The file 'missing' was not found in '"
        [".trash/files' despite being listed in '"]
        "/.trash/info'. This can be fixed by manually deleting the .trashinfo file or deleting everything (i.e. safe-rm empty -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " ".trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: [\"missing.trashinfo\"]",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: " ".trash/info/missing.trashinfo",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The file 'missing' was not found in '"
          [".trash/files' despite being listed in '"]
          ".trash/info'. This can be fixed by manually deleting the .trashinfo file or deleting everything (i.e. safe-rm empty -f)."
      ]

    -- Windows paths have backslashes which isn't valid json. Need to escape
    -- REVIEW: Is this still necessary now that we're not using json?
    escapeBackslashes [] = []
    escapeBackslashes ('\\' : xs) = '\\' : '\\' : escapeBackslashes xs
    escapeBackslashes (x : xs) = x : escapeBackslashes xs

missingInfoError :: Backend -> IO FilePath -> TestTree
missingInfoError backend args = testCase "Entry Missing Info" $ do
  testDir <- getTestPath args (withBackendDir backend "missingInfoError")
  let trashDir = testDir </> ".trash"
      argList = withSrArgs trashDir backend ["list", "--format", "m"]

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
        "The file 'bar.trashinfo' was not found in '"
        [".trash/info' despite being listed in '"]
        "/.trash/files'. This can be fixed by manually deleting the /files entry or deleting everything (i.e. safe-rm empty -f)."

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " ".trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: []",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: [\"bar\"]",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The file 'bar.trashinfo' was not found in '"
          [".trash/info' despite being listed in '"]
          ".trash/files'. This can be fixed by manually deleting the /files entry or deleting everything (i.e. safe-rm empty -f)."
      ]

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "list"
