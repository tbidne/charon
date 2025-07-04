{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for d command.
module Functional.Commands.Delete
  ( tests,
  )
where

import Charon.Data.Metadata qualified as Metadata
import Charon.Exception (DotsPathE, EmptyPathE, PathNotFound)
import Data.HashSet qualified as HashSet
import FileSystem.OsPath qualified as OsP
import Functional.Prelude

-- TODO: It would be nice if we could verify that the original location
-- is correct. Recently a bug was fixed as directories were using relative
-- paths. Evidently the tests did not catch this, presumably because
-- relative paths are sufficient here.

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Delete d Command"
    $ [ deletesOne testEnv',
        deletesTilde testEnv',
        deletesMany testEnv',
        deleteUnknownError testEnv',
        deleteDuplicateFile testEnv',
        deletesSome testEnv',
        deleteEmptyError testEnv',
        deleteDotsError testEnv'
      ]
    ++ pathologicalTests testEnv'
  where
    testEnv' = appendTestDir "delete" <$> testEnv

deletesOne :: IO TestEnv -> TestTree
deletesOne getTestEnv = testCase "Deletes one" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesOne" $ do
    testDir <- getTestDir
    let f1 = testDir </>! "f1"

    expectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    -- setup
    createFiles [f1]
    assertPathsExist [f1]
    argList <- withSrArgsM ["delete", OsP.unsafeDecode f1]

    liftIO $ runCharon argList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata

    assertFdoDirectorySizesM []
  where
    expectedMetadata = mkMetadata 1 1 0 5

-- Tests that we can correctly delete a file with a tilde in the name.
-- Actually tildes shouldn't throw errors at all because monad-effects is
-- (hopefully) performing tilde expansion for us, and any internal tildes
-- are fine.
--
-- We have a paranoia check for anything that gets through, but we probably
-- cannot test that here (which is good, because it means the upstream
-- handling works!). The internal check is tested in the unit tests.
deletesTilde :: IO TestEnv -> TestTree
deletesTilde getTestEnv = testCase "Deletes tilde" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesOne" $ do
    testDir <- getTestDir
    let f1 = testDir </>! "f~"

    expectedIdxSet <- mkPathDataSetM [("f~", PathTypeFile, 5)]

    -- setup
    createFiles [f1]
    assertPathsExist [f1]
    argList <- withSrArgsM ["delete", OsP.unsafeDecode f1]

    liftIO $ runCharon argList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata

    assertFdoDirectorySizesM []
  where
    expectedMetadata = mkMetadata 1 1 0 5

deletesMany :: IO TestEnv -> TestTree
deletesMany getTestEnv = testCase "Deletes many paths" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesMany" $ do
    testDir <- getTestDir
    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
        fileLinkToDelete = testDir </> [osp|file-link|]
        dirLinkToDelete = testDir </> [osp|dir-link|]
        linksToDelete = [fileLinkToDelete, dirLinkToDelete]

    argList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

    -- setup
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
    assertPathsExist (filesToDelete ++ dirsToDelete)
    assertSymlinksExist linksToDelete

    liftIO $ runCharon argList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
    assertSymlinksDoNotExist linksToDelete

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15),
          ("dir4", PathTypeDirectory, 10),
          ("dir-link", PathTypeSymbolicLink, 5),
          ("file-link", PathTypeSymbolicLink, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata

    assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]
  where
    expectedMetadata = mkMetadata 8 7 0 55

deleteUnknownError :: IO TestEnv -> TestTree
deleteUnknownError getTestEnv = testCase "Deletes unknown prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deleteUnknownError" $ do
    testDir <- getTestDir
    let file = testDir </>! "bad file"

    argList <- withSrArgsM ["delete", OsP.unsafeDecode file]

    -- setup
    clearDirectory testDir

    (ex, term) <- liftIO $ captureCharonExceptionTerminal @PathNotFound argList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Outfixes "Path not found:" [] "delete/deleteUnknownError/bad file'"
    expectedTerm = []

    expectedIdxSet = HashSet.fromList []
    expectedMetadata = Metadata.empty

deleteDuplicateFile :: IO TestEnv -> TestTree
deleteDuplicateFile getTestEnv = testCase "Deletes duplicate file" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deleteDuplicateFile" $ do
    testDir <- getTestDir
    let file = testDir </>! "f1"

    argList <- withSrArgsM ["delete", OsP.unsafeDecode file]

    -- setup
    clearDirectory testDir

    -- create and delete twice
    createFiles [file]
    assertPathsExist [file]
    runCharon argList

    createFiles [file]
    assertPathsExist [file]
    runCharon argList

    -- file assertions
    assertPathsDoNotExist [file]

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM2
        [ ("f1", "f1", PathTypeFile, 5),
          ("f1 (1)", "f1", PathTypeFile, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedMetadata = mkMetadata 2 2 0 10

deletesSome :: IO TestEnv -> TestTree
deletesSome getTestEnv = testCase "Deletes some files with errors" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesSome" $ do
    testDir <- getTestDir
    let realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
        filesTryDelete = (testDir </>!) <$> ["f1", "f2", "f3", "f4", "f5"]

    argList <- withSrArgsPathsM ["delete"] filesTryDelete

    -- setup
    createFiles realFiles
    assertPathsExist realFiles

    (ex, term) <- liftIO $ captureCharonExceptionTerminal @PathNotFound argList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Outfixes "Path not found:" [] "delete/deletesSome/f3'"
    expectedTerm =
      [ Exact "Deleted paths:",
        Outfixes "- " [] "delete/deletesSome/f1",
        Outfixes "- " [] "delete/deletesSome/f2",
        Exact ""
      ]
    expectedMetadata = mkMetadata 2 2 0 10

deleteEmptyError :: IO TestEnv -> TestTree
deleteEmptyError getTestEnv = testCase "Deletes empty prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deleteEmptyError" $ do
    testDir <- getTestDir

    argList <- withSrArgsM ["delete", ""]

    -- setup
    clearDirectory testDir

    (ex, term) <- liftIO $ captureCharonExceptionTerminal @EmptyPathE argList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Exact "Attempted to delete the empty path! This is not allowed."
    expectedTerm = []

    expectedIdxSet = HashSet.fromList []
    expectedMetadata = Metadata.empty

deleteDotsError :: IO TestEnv -> TestTree
deleteDotsError getTestEnv = testCase "Deletes dots prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deleteDotsError" $ do
    testDir <- getTestDir
    let dots = [[osp|.|], [osp|..|], [osp|...|]]
        files = (testDir </>) <$> dots

    argList <- withSrArgsM $ "delete" : (OsP.unsafeDecode <$> files)

    -- setup
    clearDirectory testDir

    (ex, term) <- liftIO $ captureCharonExceptionTerminal @DotsPathE argList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedEx =
      Outfixes
        "Attempted to delete the special path"
        []
        ".'! This is not allowed."
    expectedTerm =
      []

    expectedIdxSet = HashSet.fromList []
    expectedMetadata = Metadata.empty

pathologicalTests :: IO TestEnv -> [TestTree]

#if OSX

pathologicalTests testEnv = [deletesPathological2 testEnv]

#else

pathologicalTests testEnv =
  [ deletesPathological1 testEnv,
    deletesPathological2 testEnv
  ]

deletesPathological1 :: IO TestEnv -> TestTree
deletesPathological1 getTestEnv = testCase "Deletes pathological files 1" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesPathological1" $ do
    testDir <- getTestDir

    -- See NOTE: [Unicode normalization]
    --
    -- This test was originally written as an example of the kind of test
    -- that causes OSX integration tests to fail, and something we needed
    -- to fix. That is, the idea is that we would eventually have this test
    -- passing on OSX too.
    --
    --However, we came around to the opinion that this was merely a
    -- test failure, not anything wrong with our behavior wrt OSX. Thus there
    -- is not much need for this test anymore, though it does serve as
    -- documentation, so we leave it for now.
    let files =
          (testDir </>!)
            <$> [ "\8061", -- ώ, These two are __not__ the same (compare /=)
                  "\974" -- ώ
                ]

    argList <- withSrArgsPathsM ["delete"] files

    -- setup
    createFiles files
    assertPathsExist files

    liftIO $ runCharon argList

    -- file assertions
    assertPathsDoNotExist files

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ ("ώ", PathTypeFile, 5),
          ("ώ", PathTypeFile, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedMetadata = mkMetadata 2 2 0 10

#endif

deletesPathological2 :: IO TestEnv -> TestTree
deletesPathological2 getTestEnv = testCase "Deletes pathological files 2" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesPathological2" $ do
    testDir <- getTestDir
    let files =
          (testDir </>!)
            <$> [ "\32244", -- 練
                  "\4096", -- က
                  "\13312" -- 㐀
                ]

    argList <- withSrArgsPathsM ["delete"] files

    -- setup
    createFiles files
    assertPathsExist files

    liftIO $ runCharon argList

    -- file assertions
    assertPathsDoNotExist files

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ ("練", PathTypeFile, 5),
          ("က", PathTypeFile, 5),
          ("㐀", PathTypeFile, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
    assertFdoDirectorySizesM []
  where
    expectedMetadata = mkMetadata 3 3 0 15
