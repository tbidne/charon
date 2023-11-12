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

    result <- captureSafeRm argList

    assertMatches expectedTerminal result
  where
    expectedTerminal =
      [ Exact ""
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

    ex <- captureSafeRmException @TrashDirFilesNotFoundE argList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfix
        "The trash files directory was not found at '"
        (trashFiles <> "' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f).")

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

    ex <- captureSafeRmException @TrashDirInfoNotFoundE argList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfix
        "The trash info directory was not found at '"
        (trashInfo <> "' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. safe-rm empty -f).")

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
    ex <- captureSafeRmException @TrashEntryFileNotFoundE listArgList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfixes
        "The file 'missing' was not found in '"
        [trashFiles <> "' despite being listed in '"]
        (trashInfo <> "'. This can be fixed by manually deleting the info file or deleting everything (i.e. safe-rm empty -f).")

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

    ex <- captureSafeRmException @TrashEntryInfoNotFoundE argList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfixes
        "The file 'bar.<ext>' was not found in '"
        [trashInfo <> "' despite being listed in '"]
        (trashFiles <> "'. This can be fixed by manually deleting the 'files' entry or deleting everything (i.e. safe-rm empty -f).")

trashFiles :: Text
trashFiles = T.pack (".trash" `cfp` "files")

trashInfo :: Text
trashInfo = T.pack (".trash" `cfp` "info")
