-- | Tests for d command.
module Functional.Commands.Delete
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Effectful.FileSystem.Utils (unsafeDecodeOsToFp)
import Functional.Prelude
import SafeRm.Data.Metadata
  ( Metadata
      ( MkMetadata,
        logSize,
        numEntries,
        numFiles,
        size
      ),
  )
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Exception (FileNotFoundE)

-- TODO: It would be nice if we could verify that the original location
-- is correct. Recently a bug was fixed as directories were using relative
-- paths. Evidently the tests did not catch this, presumably because
-- relative paths are sufficient here.

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Delete Command"
    [ deletesOne testEnv',
      deletesMany testEnv',
      deleteUnknownError testEnv',
      deleteDuplicateFile testEnv',
      deletesSome testEnv'
    ]
  where
    testEnv' = appendTestDir "delete" <$> testEnv

deletesOne :: IO TestEnv -> TestTree
deletesOne getTestEnv = testCase "Deletes one" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "deletesOne" $ do
    testDir <- getTestDir
    let trashDir = testDir </> (testEnv ^. #trashDir)
        f1 = testDir </>! "f1"

    expectedIdxSet <- mkPathDataSetM ["f1"]

    -- setup
    createFiles [f1]
    assertPathsExist [f1]
    argList <- withSrArgsM ["delete", unsafeDecodeOsToFp f1]

    runSafeRm argList

    -- file assertions
    trashPaths <- mkAllTrashPathsM ["f1"]
    assertPathsExist (trashDir : trashPaths)
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    expectedMetadata @=? metadata
  where
    expectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

deletesMany :: IO TestEnv -> TestTree
deletesMany getTestEnv = testCase "Deletes many paths" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "deletesMany" $ do
    testDir <- getTestDir
    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]

    argList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

    -- setup
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    assertPathsExist (filesToDelete ++ dirsToDelete)

    runSafeRm argList

    -- file assertions
    fileTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir1", "dir2"]
    assertPathsExist fileTrashPaths
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "dir1",
          "dir2"
        ]

    assertSetEq expectedIdxSet idxSet
    expectedMetadata @=? metadata
  where
    expectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

deleteUnknownError :: IO TestEnv -> TestTree
deleteUnknownError getTestEnv = testCase "Deletes unknown prints error" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "deleteUnknownError" $ do
    testDir <- getTestDir
    let file = testDir </>! "bad file"

    argList <- withSrArgsM ["delete", unsafeDecodeOsToFp file]

    -- setup
    clearDirectory testDir

    (ex, _) <- captureSafeRmExceptionLogs @FileNotFoundE argList

    assertMatch expectedEx ex

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    expectedMetadata @=? metadata
  where
    expectedEx =
      Outfixes
        "File not found: "
        [combineFps ["deleteUnknownError-"]]
        "bad file"

    expectedIdxSet = HashSet.fromList []
    expectedMetadata = Metadata.empty

deleteDuplicateFile :: IO TestEnv -> TestTree
deleteDuplicateFile getTestEnv = testCase "Deletes duplicate file" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "deleteDuplicateFile" $ do
    testDir <- getTestDir
    let trashDir = testDir </> (testEnv ^. #trashDir)
        file = testDir </>! "f1"

    argList <- withSrArgsM ["delete", unsafeDecodeOsToFp file]

    -- setup
    clearDirectory testDir

    -- create and delete twice
    createFiles [file]
    assertPathsExist [file]
    runSafeRm argList

    createFiles [file]
    assertPathsExist [file]
    runSafeRm argList

    -- file assertions
    fileTrashPaths <- mkAllTrashPathsM ["f1 (1)", "f1"]
    assertPathsExist (trashDir : fileTrashPaths)
    assertPathsDoNotExist [file]

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM2
        [ ("f1", "f1"),
          ("f1 (1)", "f1")
        ]

    assertSetEq expectedIdxSet idxSet
    expectedMetadata @=? metadata
  where
    expectedMetadata =
      MkMetadata
        { numEntries = 2,
          numFiles = 2,
          logSize = afromInteger 0,
          size = afromInteger 10
        }

deletesSome :: IO TestEnv -> TestTree
deletesSome getTestEnv = testCase "Deletes some files with errors" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "deletesSome" $ do
    testDir <- getTestDir
    let realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
        filesTryDelete = (testDir </>!) <$> ["f1", "f2", "f3", "f4", "f5"]

    argList <- withSrArgsPathsM ["delete"] filesTryDelete

    -- setup
    createFiles realFiles
    assertPathsExist realFiles

    (ex, _) <- captureSafeRmExceptionLogs @FileNotFoundE argList

    -- file assertions
    trashPaths1 <- mkAllTrashPathsM ["f1", "f2", "f5"]
    assertPathsExist trashPaths1
    trashPaths2 <- mkAllTrashPathsM ["f3", "f4"]
    assertPathsDoNotExist trashPaths2

    assertMatch expectedEx ex

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f5"
        ]

    assertSetEq expectedIdxSet idxSet
    expectedMetadata @=? metadata
  where
    expectedEx =
      Outfixes
        "File not found: "
        [combineFps ["deletesSome-"]]
        "/f4"
    expectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 15
        }

combineFps :: [FilePath] -> Text
combineFps =
  T.pack
    . foldFilePathsAcc ("safe-rm" `cfp` "functional" `cfp` "delete")
