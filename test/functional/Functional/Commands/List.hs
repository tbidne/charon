-- | Tests for l command.
module Functional.Commands.List
  ( tests,
  )
where

import Data.Text qualified as T
import Effects.FileSystem.PathWriter qualified as PW
import Functional.Prelude
import SafeRm.Exception
  ( TrashDirFilesNotFoundE,
    TrashDirInfoNotFoundE,
    TrashEntryFileNotFoundE,
    TrashEntryInfoNotFoundE,
  )

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "List Command"
    [ emptySucceeds testEnv',
      noPathsError testEnv',
      noInfoError testEnv',
      missingPathError testEnv',
      missingInfoError testEnv'
    ]
  where
    testEnv' = appendTestDir "list" <$> testEnv

emptySucceeds :: IO TestEnv -> TestTree
emptySucceeds getTestEnv = testCase "List on empty directory succeeds" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptySucceeds" $ do
    argList <- withSrArgsM ["list", "--format", "m"]

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

noPathsError :: IO TestEnv -> TestTree
noPathsError getTestEnv = testCase "No Paths Error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "noPathsError" $ do
    testDir <- getTestDir

    let trashDir = testDir </> pathDotTrash
    argList <- withSrArgsM ["list", "--format", "m"]

    -- setup
    clearDirectory testDir
    clearDirectory trashDir
    clearDirectory (trashDir </> pathInfo)

    (ex, logs) <- captureSafeRmExceptionLogs @TrashDirFilesNotFoundE argList

    assertMatch expectedErr ex
    assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix
        "The trash files directory was not found at '"
        (trashFiles <> "' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f).")

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash files directory was not found at '" ".trash/files' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]

noInfoError :: IO TestEnv -> TestTree
noInfoError getTestEnv = testCase "No Info Error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "noInfoError" $ do
    testDir <- getTestDir

    let trashDir = testDir </> pathDotTrash
    argList <- withSrArgsM ["list", "--format", "m"]

    -- setup
    clearDirectory testDir
    clearDirectory trashDir
    clearDirectory (trashDir </> pathFiles)

    (ex, logs) <- captureSafeRmExceptionLogs @TrashDirInfoNotFoundE argList

    assertMatch expectedErr ex
    assertMatches expectedLogs logs
  where
    expectedErr =
      Outfix
        "The trash info directory was not found at '"
        (trashInfo <> "' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f).")

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The trash info directory was not found at '" ".trash/info' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]

missingPathError :: IO TestEnv -> TestTree
missingPathError getTestEnv = testCase "Entry Missing Path" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "missingPathError" $ do
    testDir <- getTestDir

    let trashDir = testDir </> pathDotTrash
        missing = testDir </>! "missing"

    -- SETUP

    -- clearDirectory testDir
    createFileContents [(missing, "")]

    delArgList <- withSrArgsPathsM ["delete"] [missing]
    runSafeRm delArgList

    -- delete file from trash for expected error
    PW.removeFile (trashDir </> pathFiles </>! "missing")

    -- Creating empty file so that we don't get the "size mismatch" error.
    -- We specifically want the "missing.trashinfo has no corresponding missing" error.
    createFiles [trashDir </> pathFiles </>! "blah"]

    listArgList <- withSrArgsM ["list", "--format", "m"]
    (ex, logs) <- captureSafeRmExceptionLogs @TrashEntryFileNotFoundE listArgList

    assertMatch expectedErr ex
    assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The file 'missing' was not found in '"
        [trashFiles <> "' despite being listed in '"]
        (trashInfo <> "'. This can be fixed by manually deleting the info file or deleting everything (i.e. safe-rm empty -f).")

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " ".trash/info",
        Prefix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info: missing.",
        Outfixes "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Path: " [".trash/info/missing."] "",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The file 'missing' was not found in '"
          [".trash/files' despite being listed in '"]
          ".trash/info'. This can be fixed by manually deleting the info file or deleting everything (i.e. safe-rm empty -f)."
      ]

missingInfoError :: IO TestEnv -> TestTree
missingInfoError getTestEnv = testCase "Entry Missing Info" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "missingInfoError" $ do
    testDir <- getTestDir

    let trashDir = testDir </> pathDotTrash
    argList <- withSrArgsM ["list", "--format", "m"]

    -- setup
    clearDirectory testDir
    clearDirectory trashDir
    clearDirectory (trashDir </> pathFiles)
    clearDirectory (trashDir </> pathInfo)
    createFiles [trashDir </> pathFiles </>! "bar"]

    (ex, logs) <- captureSafeRmExceptionLogs @TrashEntryInfoNotFoundE argList

    assertMatch expectedErr ex
    assertMatches expectedLogs logs
  where
    expectedErr =
      Outfixes
        "The file 'bar.<ext>' was not found in '"
        [trashInfo <> "' despite being listed in '"]
        (trashFiles <> "'. This can be fixed by manually deleting the 'files' entry or deleting everything (i.e. safe-rm empty -f).")

    expectedLogs =
      [ Outfix "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs] Trash home: " ".trash",
        Outfix "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Trash info: " ".trash/info",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Info:",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs] Paths: bar",
        Outfixes
          "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs] The file 'bar.<ext>' was not found in '"
          [".trash/info' despite being listed in '"]
          ".trash/files'. This can be fixed by manually deleting the 'files' entry or deleting everything (i.e. safe-rm empty -f)."
      ]

trashFiles :: Text
trashFiles = T.pack (".trash" `cfp` "files")

trashInfo :: Text
trashInfo = T.pack (".trash" `cfp` "info")
