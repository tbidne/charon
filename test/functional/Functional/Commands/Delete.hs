{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for d command.
module Functional.Commands.Delete
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Effects.FileSystem.Utils (unsafeDecodeOsToFp)
import Functional.Prelude
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
    $ [ deletesOne testEnv',
        deletesMany testEnv',
        deleteUnknownError testEnv',
        deleteDuplicateFile testEnv',
        deletesSome testEnv'
      ]
    ++ (pathologicalTests testEnv')
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
    argList <- withSrArgsM ["delete", unsafeDecodeOsToFp f1]

    liftIO $ runSafeRm argList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
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

    liftIO $ runSafeRm argList

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
          ("dir-link", PathTypeSymlink, 5),
          ("file-link", PathTypeSymlink, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
  where
    expectedMetadata = mkMetadata 8 7 0 55

deleteUnknownError :: IO TestEnv -> TestTree
deleteUnknownError getTestEnv = testCase "Deletes unknown prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deleteUnknownError" $ do
    testDir <- getTestDir
    let file = testDir </>! "bad file"

    argList <- withSrArgsM ["delete", unsafeDecodeOsToFp file]

    -- setup
    clearDirectory testDir

    (ex, _) <- liftIO $ captureSafeRmExceptionLogs @FileNotFoundE argList

    assertMatch expectedEx ex

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
  where
    expectedEx =
      Outfixes
        "File not found: "
        [combineFps ["deleteUnknownError"]]
        "bad file'"

    expectedIdxSet = HashSet.fromList []
    expectedMetadata = Metadata.empty

deleteDuplicateFile :: IO TestEnv -> TestTree
deleteDuplicateFile getTestEnv = testCase "Deletes duplicate file" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deleteDuplicateFile" $ do
    testDir <- getTestDir
    let file = testDir </>! "f1"

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

    (ex, _) <- liftIO $ captureSafeRmExceptionLogs @FileNotFoundE argList

    assertMatch expectedEx ex

    -- trash structure assertions
    (idxSet, metadata) <- runIndexMetadataM

    expectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f5", PathTypeFile, 5)
        ]

    assertSetEq expectedIdxSet idxSet
    liftIO $ expectedMetadata @=? metadata
  where
    expectedEx =
      Outfixes
        "File not found: "
        [combineFps ["deletesSome"]]
        "/f4'"
    expectedMetadata = mkMetadata 3 3 0 15

pathologicalTests :: IO TestEnv -> [TestTree]

#if OSX

pathologicalTests _ = []

#else

pathologicalTests testEnv = [deletesPathological testEnv]

deletesPathological :: IO TestEnv -> TestTree
deletesPathological getTestEnv = testCase "Deletes pathological files" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesPathological" $ do
    testDir <- getTestDir

    -- FIXME: The following paths were generated in the int tests and caused
    -- a failure on OSX. For some reason, OSX believes the below two paths are
    -- the same! That is, we first delete ώ (8061) and then attempt to delete
    -- ώ (974), yet SafeRm.Backend.Default.Utils.mkUniqPath believes this path
    -- already exists. Thus it creates
    --
    --   ώ (1)
    --
    -- The tests do not expect this, and it fails.
    --
    -- More info on why this occurs: According to the stackoverflow answer
    --
    --     https://stackoverflow.com/questions/38484369/why-does-the-c-runtime-on-mac-os-allow-both-precomposed-and-decomposed-utf-8
    --
    -- OSX "normalizes" paths per UTF-8 standards. In particular, two unique
    -- encodings that "represent" the same thing (e.g. characters with accents)
    -- can end up identical.
    --
    -- In our case, because these two glyphs look identical, they likely
    -- share the same normalization, hence the OSX failure.
    --
    -- OSX is arguably doing the right thing here, so maybe we should too.
    --
    -- The way to fix this would be to normalize paths ourselves (probably do
    -- this on initial CLI parsing), and then normalize paths when we generate
    -- them in the tests.
    --
    -- See the text-icu package:
    --
    -- https://hackage.haskell.org/package/text-icu-0.8.0.4/docs/Data-Text-ICU-Normalize.html#v:normalize
    --
    -- And also unicode-transforms.
    let files =
          (testDir </>!)
            <$> [ "\8061", -- ώ, These two are __not__ the same (compare /=)
                  "\974" -- ώ
                ]

    argList <- withSrArgsPathsM ["delete"] files

    -- setup
    createFiles files
    assertPathsExist files

    liftIO $ runSafeRm argList

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
  where
    expectedMetadata = mkMetadata 2 2 0 10

#endif

combineFps :: [FilePath] -> Text
combineFps =
  T.pack
    . foldFilePathsAcc "delete"
