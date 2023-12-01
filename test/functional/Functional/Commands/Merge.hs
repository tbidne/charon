{-# LANGUAGE QuasiQuotes #-}

-- | Tests for merge command.
module Functional.Commands.Merge
  ( tests,
  )
where

import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Merge Command"
    [ mergeSucceeds testEnv',
      mergeCollisionFails testEnv'
    ]
  where
    testEnv' = appendTestDir "merge" <$> testEnv

mergeSucceeds :: IO TestEnv -> TestTree
mergeSucceeds getTestEnv = testCase "Merge succeeds" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "mergeSucceeds" $ do
    testDir <- getTestDir

    -- SETUP SRC

    local (set' #trashDir pathSrc) $ do
      let filesToDelete = (testDir </>!) <$> ["sf1", "sf2", "sf3"]
          dirsToDelete = (testDir </>!) <$> ["sdir1", "sdir2"]
      delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

      createDirectories ((testDir </>!) <$> ["sdir1", "sdir2", "sdir2/sdir3"])
      createFiles ((testDir </>! "sdir2/sdir3/foo") : filesToDelete)
      assertPathsExist (filesToDelete ++ dirsToDelete)

      runSafeRm delArgList

      -- file assertions
      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ ("sf1", PathTypeFile, 5),
            ("sf2", PathTypeFile, 5),
            ("sf3", PathTypeFile, 5),
            ("sdir1", PathTypeDirectory, 5),
            ("sdir2", PathTypeDirectory, 15)
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata
      assertFdoDirectorySizesM ["sdir1", "sdir2"]

    -- SETUP DEST

    local (set' #trashDir pathDest) $ do
      let filesToDelete = (testDir </>!) <$> ["df1", "df2", "df3"]
          dirsToDelete = (testDir </>!) <$> ["ddir1", "ddir2"]
      delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

      createDirectories ((testDir </>!) <$> ["ddir1", "ddir2", "ddir2/ddir3"])
      createFiles ((testDir </>! "ddir2/ddir3/foo") : filesToDelete)
      assertPathsExist (filesToDelete ++ dirsToDelete)

      runSafeRm delArgList

      -- file assertions
      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ ("df1", PathTypeFile, 5),
            ("df2", PathTypeFile, 5),
            ("df3", PathTypeFile, 5),
            ("ddir1", PathTypeDirectory, 5),
            ("ddir2", PathTypeDirectory, 15)
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata
      assertFdoDirectorySizesM ["ddir1", "ddir2"]

    -- MERGE FROM SRC TO DEST
    local (set' #trashDir pathSrc) $ do
      let dest = testDir </> pathDest
      mergeArgs <- withSrArgsPathsM ["merge", "-d"] [dest]
      runSafeRm mergeArgs

    -- VERIFY DEST
    local (set' #trashDir pathDest) $ do
      -- trash structure assertions
      mergeExpectedIdxSet <-
        mkPathDataSetM
          [ ("sf1", PathTypeFile, 5),
            ("sf2", PathTypeFile, 5),
            ("sf3", PathTypeFile, 5),
            ("df1", PathTypeFile, 5),
            ("df2", PathTypeFile, 5),
            ("df3", PathTypeFile, 5),
            ("sdir1", PathTypeDirectory, 5),
            ("sdir2", PathTypeDirectory, 15),
            ("ddir1", PathTypeDirectory, 5),
            ("ddir2", PathTypeDirectory, 15)
          ]

      (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataM

      assertSetEq mergeExpectedIdxSet mergeIdxSetDest
      mergeExpectedMetadata @=? mergeMetadataDest
      assertFdoDirectorySizesTestDirM testDir ["sdir1", "sdir2", "ddir1", "ddir2"]
  where
    delExpectedMetadata = mkMetadata 5 4 0 35

    mergeExpectedMetadata = mkMetadata 10 8 0 70

mergeCollisionFails :: IO TestEnv -> TestTree
mergeCollisionFails getTestEnv = testCase "Merge fails due to collision" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "mergeCollisionFails" $ do
    testDir <- getTestDir

    let trashDirDest = testDir </>! "dest"

    -- SETUP SRC

    pathsToDeleteSrc <- local (set' #trashDir pathSrc) $ do
      let filesToDelete = (testDir </>!) <$> ["sf1", "sf2", "sf3"]
          dirsToDelete = (testDir </>!) <$> ["sdir1", "dir2"]

      delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)
      -- NOTE: both trash dirs have dir2, giving us the collision we need
      createDirectories ((testDir </>!) <$> ["sdir1", "dir2", "dir2/sdir3"])
      createFiles ((testDir </>! "dir2/sdir3/foo") : filesToDelete)

      runSafeRm delArgList

      -- file assertions
      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ ("sf1", PathTypeFile, 5),
            ("sf2", PathTypeFile, 5),
            ("sf3", PathTypeFile, 5),
            ("sdir1", PathTypeDirectory, 5),
            ("dir2", PathTypeDirectory, 15)
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata
      assertFdoDirectorySizesM ["sdir1", "dir2"]

      pure (filesToDelete ++ dirsToDelete)

    -- SETUP DEST

    (pathsToDeleteDest, delExpectedIdxSetDest) <-
      local (set' #trashDir pathDest) $ do
        let filesToDelete = (testDir </>!) <$> ["df1", "df2", "df3"]
            dirsToDelete = (testDir </>!) <$> ["ddir1", "dir2"]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)
        -- NOTE: both trash dirs have dir2, giving us the collision we need
        createDirectories ((testDir </>!) <$> ["ddir1", "dir2", "dir2/sdir3"])
        createFiles ((testDir </>! "dir2/sdir3/foo") : filesToDelete)

        runSafeRm delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        delExpectedIdxSet <-
          mkPathDataSetM
            [ ("df1", PathTypeFile, 5),
              ("df2", PathTypeFile, 5),
              ("df3", PathTypeFile, 5),
              ("ddir1", PathTypeDirectory, 5),
              ("dir2", PathTypeDirectory, 15)
            ]

        (delIdxSet, delMetadata) <- runIndexMetadataM
        assertSetEq delExpectedIdxSet delIdxSet
        delExpectedMetadata @=? delMetadata
        assertFdoDirectorySizesM ["ddir1", "dir2"]

        pure (filesToDelete ++ dirsToDelete, delExpectedIdxSet)

    -- MERGE

    local (set' #trashDir pathSrc) $ do
      mergeArgs <- withSrArgsPathsM ["merge", "-d"] [trashDirDest]
      runSafeRmException @IOException mergeArgs

      -- verify src unchanged
      assertPathsDoNotExist (pathsToDeleteSrc ++ pathsToDeleteDest)

    local (set' #trashDir pathDest) $ do
      -- verify dest unchanged
      (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataM
      assertSetEq delExpectedIdxSetDest mergeIdxSetDest
      delExpectedMetadata @=? mergeMetadataDest
      assertFdoDirectorySizesM ["ddir1", "dir2"]
  where
    delExpectedMetadata = mkMetadata 5 4 0 35

pathSrc :: OsPath
pathSrc = [osp|src|]

pathDest :: OsPath
pathDest = [osp|dest|]
