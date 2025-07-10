{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for d command.
module Functional.Commands.Delete
  ( tests,
  )
where

import Charon.Exception (DotsPathE, EmptyPathE, PathNotFound)
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
deletesOne getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes one",
        testName = testDirPrefix <> [osp|deletesOne|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesOne" $ do
        testDir <- getTestDir
        let f1 = testDir </> [osstr|f1|]

        -- setup
        createFiles [f1]
        assertPathsExist [f1]
        argList <- withSrArgsM ["delete", "-v", OsP.unsafeDecode f1]

        liftIO $ runCharon argList

        -- file assertions
        assertPathsDoNotExist [f1]

        captureIndexBs testDir

-- Tests that we can correctly delete a file with a tilde in the name.
-- Actually tildes shouldn't throw errors at all because monad-effects is
-- (hopefully) performing tilde expansion for us, and any internal tildes
-- are fine.
--
-- We have a paranoia check for anything that gets through, but we probably
-- cannot test that here (which is good, because it means the upstream
-- handling works!). The internal check is tested in the unit tests.
deletesTilde :: IO TestEnv -> TestTree
deletesTilde getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes tilde",
        testName = testDirPrefix <> [osp|deletesTilde|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesTilde" $ do
        testDir <- getTestDir
        let f1 = testDir </>! "f~"

        -- setup
        createFiles [f1]
        assertPathsExist [f1]
        argList <- withSrArgsM ["delete", "-v", OsP.unsafeDecode f1]

        liftIO $ runCharon argList

        -- file assertions
        assertPathsDoNotExist [f1]

        captureIndexBs testDir

{- ORMOLU_DISABLE -}

deletesMany :: IO TestEnv -> TestTree
deletesMany getTestEnv =
  testGoldenParamsOs
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes many paths",
        testName = testDirPrefix <> [osp|deletesMany|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesMany" $ do
        testDir <- getTestDir
        -- Tests trailing slash and trailing whitespace. We choose a filename
        -- (e_ws) that will not be last, since our test suite strips trailing
        -- whitespace, but we want to see it.
        --
        -- Cannot have this test on windows as trailing whitespace is stripped.
#if WINDOWS
        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
#else
        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3", "e_ws  "]
#endif
      
        let dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4", "dirslash/"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        argList <- withSrArgsPathsM ["delete", "-v"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- setup
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4", "dirslash/"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
        assertPathsExist (filesToDelete ++ dirsToDelete)
        assertSymlinksExist linksToDelete

        liftIO $ runCharon argList

        let duplicate = [testDir </>! "f1"]
        argListDuplicate <- withSrArgsPathsM ["delete", "-v"] duplicate
        createFiles duplicate
        assertPathsExist duplicate

        liftIO $ runCharon argListDuplicate

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ duplicate)
        assertSymlinksDoNotExist linksToDelete

        captureIndexBs testDir

{- ORMOLU_ENABLE -}

deleteUnknownError :: IO TestEnv -> TestTree
deleteUnknownError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes unknown prints error",
        testName = testDirPrefix <> [osp|deleteUnknownError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deleteUnknownError" $ do
        testDir <- getTestDir
        let file = testDir </> [osstr|bad file|]

        argList <- withSrArgsM ["delete", "-v", OsP.unsafeDecode file]

        -- setup
        clearDirectory testDir

        captureCharonTermBsE @PathNotFound testDir argList

deleteDuplicateFile :: IO TestEnv -> TestTree
deleteDuplicateFile getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes duplicate file",
        testName = testDirPrefix <> [osp|deleteDuplicateFile|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deleteDuplicateFile" $ do
        testDir <- getTestDir
        let file = testDir </> [osstr|f1|]

        argList <- withSrArgsM ["delete", "-v", OsP.unsafeDecode file]

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

        assertFdoDirectorySizesM []
        captureIndexBs testDir

deletesSome :: IO TestEnv -> TestTree
deletesSome getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes some files with errors",
        testName = testDirPrefix <> [osp|deletesSome|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesSome" $ do
        testDir <- getTestDir
        let realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
            filesTryDelete = (testDir </>!) <$> ["f1", "f2", "f3", "f4", "f5"]

        argList <- withSrArgsPathsM ["delete", "-v"] filesTryDelete

        -- setup
        createFiles realFiles
        assertPathsExist realFiles

        term <- captureCharonTermBsE @PathNotFound testDir argList

        -- trash structure assertions
        assertFdoDirectorySizesM []
        indexMeta <- captureIndexBs testDir

        pure $ term <> indexMeta

deleteEmptyError :: IO TestEnv -> TestTree
deleteEmptyError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes empty prints error",
        testName = testDirPrefix <> [osp|deleteEmptyError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deleteEmptyError" $ do
        testDir <- getTestDir

        argList <- withSrArgsM ["delete", "-v", ""]

        -- setup
        clearDirectory testDir

        -- trash structure assertions
        bs <- captureCharonTermBsE @EmptyPathE testDir argList
        assertFdoDirectorySizesM []
        pure bs

deleteDotsError :: IO TestEnv -> TestTree
deleteDotsError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes dots prints error",
        testName = testDirPrefix <> [osp|deleteDotsError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deleteDotsError" $ do
        testDir <- getTestDir
        let dots = [[osp|.|], [osp|..|], [osp|...|]]
            files = (testDir </>) <$> dots

        argList <- withSrArgsM $ "delete" : "-v" : (OsP.unsafeDecode <$> files)

        -- setup
        clearDirectory testDir

        bs <- captureCharonTermBsE @DotsPathE testDir argList
        assertFdoDirectorySizesM []
        pure bs

pathologicalTests :: IO TestEnv -> [TestTree]

#if OSX

pathologicalTests testEnv = [deletesPathological2 testEnv]

#else

pathologicalTests testEnv =
  [ deletesPathological1 testEnv,
    deletesPathological2 testEnv
  ]

deletesPathological1 :: IO TestEnv -> TestTree
deletesPathological1 getTestEnv =
  -- Interestingly, the index order is different on windows and linux, hence
  -- separate golden files.
  testGoldenParamsOs
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes pathological files 1",
        testName = testDirPrefix <> [osp|deletesPathological1|]
      }
  where
    runner = do
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

        argList <- withSrArgsPathsM ["delete", "-v"] files

        -- setup
        createFiles files
        assertPathsExist files

        liftIO $ runCharon argList

        -- file assertions
        assertPathsDoNotExist files

        -- trash structure assertions
        assertFdoDirectorySizesM []
        captureIndexBs testDir

#endif

deletesPathological2 :: IO TestEnv -> TestTree
deletesPathological2 getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes pathological files 2",
        testName = testDirPrefix <> [osp|deletesPathological2|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesPathological2" $ do
        testDir <- getTestDir
        let files =
              (testDir </>!)
                <$> [ "\32244", -- 練
                      "\4096", -- က
                      "\13312" -- 㐀
                    ]

        argList <- withSrArgsPathsM ["delete", "-v"] files

        -- setup
        createFiles files
        assertPathsExist files

        liftIO $ runCharon argList

        -- file assertions
        assertPathsDoNotExist files

        -- trash structure assertions
        assertFdoDirectorySizesM []
        captureIndexBs testDir

testDirPrefix :: OsString
testDirPrefix = [osstr|delete_|]
