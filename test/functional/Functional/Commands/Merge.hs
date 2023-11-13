{-# LANGUAGE QuasiQuotes #-}

-- | Tests for merge command.
module Functional.Commands.Merge
  ( tests,
  )
where

import Effects.FileSystem.PathWriter (PathExistsException)
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

      -- lookup assertions
      lookupArgs <- withSrArgsM ["lookup", "*"]
      lookupResult <- liftIO $ captureSafeRm lookupArgs
      expectedLookup <-
        mkLookupDirSize ["sf1", "sf2", "sf3"] [("sdir1", Nothing), ("sdir2", Just "15.00B")]
      assertMatches expectedLookup lookupResult

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ "sf1",
            "sf2",
            "sf3",
            "sdir1",
            "sdir2"
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata

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

      -- lookup assertions
      lookupArgs <- withSrArgsM ["lookup", "*"]
      lookupResult <- liftIO $ captureSafeRm lookupArgs
      expectedLookup <- mkLookupDirSize ["df1", "df2", "df3"] [("ddir1", Nothing), ("ddir2", Just "15.00B")]
      assertMatches expectedLookup lookupResult

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ "df1",
            "df2",
            "df3",
            "ddir1",
            "ddir2"
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata

    -- MERGE FROM SRC TO DEST
    local (set' #trashDir pathSrc) $ do
      let dest = testDir </> pathDest
      mergeArgs <- withSrArgsPathsM ["merge", "-d"] [dest]
      runSafeRm mergeArgs

    -- VERIFY DEST
    local (set' #trashDir pathDest) $ do
      -- lookup assertions
      lookupArgsS <- withSrArgsM ["lookup", "s*"]
      lookupResultS <- liftIO $ captureSafeRm lookupArgsS
      expectedLookupS <-
        mkLookupDirSize ["sf1", "sf2", "sf3"] [("sdir1", Nothing), ("sdir2", Just "15.00B")]
      assertMatches expectedLookupS lookupResultS

      lookupArgsD <- withSrArgsM ["lookup", "d*"]
      lookupResultD <- liftIO $ captureSafeRm lookupArgsD
      expectedLookupD <-
        mkLookupDirSize ["df1", "df2", "df3"] [("ddir1", Nothing), ("ddir2", Just "15.00B")]
      assertMatches expectedLookupD lookupResultD

      -- trash structure assertions
      mergeExpectedIdxSet <-
        mkPathDataSetM
          [ "sf1",
            "sf2",
            "sf3",
            "df1",
            "df2",
            "df3",
            "sdir1",
            "sdir2",
            "ddir1",
            "ddir2"
          ]

      (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataM

      assertSetEq mergeExpectedIdxSet mergeIdxSetDest
      mergeExpectedMetadata @=? mergeMetadataDest
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

    mergeExpectedMetadata =
      MkMetadata
        { numEntries = 10,
          numFiles = 8,
          logSize = afromInteger 0,
          size = afromInteger 40
        }

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

      -- lookup assertions
      lookupArgs <- withSrArgsM ["lookup", "*"]
      lookupResult <- liftIO $ captureSafeRm lookupArgs
      expectedLookup <-
        mkLookupDirSize ["sf1", "sf2", "sf3"] [("dir2", Just "15.00B"), ("sdir1", Nothing)]
      assertMatches expectedLookup lookupResult

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ "sf1",
            "sf2",
            "sf3",
            "sdir1",
            "dir2"
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata

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

        -- lookup assertions
        lookupArgs <- withSrArgsM ["lookup", "*"]
        lookupResult <- liftIO $ captureSafeRm lookupArgs
        expectedLookup <-
          mkLookupDirSize ["df1", "df2", "df3"] [("ddir1", Nothing), ("dir2", Just "15.00B")]
        assertMatches expectedLookup lookupResult

        -- trash structure assertions
        delExpectedIdxSet <-
          mkPathDataSetM
            [ "df1",
              "df2",
              "df3",
              "ddir1",
              "dir2"
            ]

        (delIdxSet, delMetadata) <- runIndexMetadataM
        assertSetEq delExpectedIdxSet delIdxSet
        delExpectedMetadata @=? delMetadata

        pure (filesToDelete ++ dirsToDelete, delExpectedIdxSet)

    -- MERGE

    local (set' #trashDir pathSrc) $ do
      mergeArgs <- withSrArgsPathsM ["merge", "-d"] [trashDirDest]
      runSafeRmException @PathExistsException mergeArgs

      -- verify src unchanged
      assertPathsDoNotExist (pathsToDeleteSrc ++ pathsToDeleteDest)

      -- lookup assertions
      lookupArgs <- withSrArgsM ["lookup", "*"]
      lookupResult <- liftIO $ captureSafeRm lookupArgs
      expectedLookup <-
        mkLookupDirSize ["sf1", "sf2", "sf3"] [("dir2", Just "15.00B"), ("sdir1", Nothing)]
      assertMatches expectedLookup lookupResult

    local (set' #trashDir pathDest) $ do
      -- verify dest unchanged
      (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataM
      assertSetEq delExpectedIdxSetDest mergeIdxSetDest
      delExpectedMetadata @=? mergeMetadataDest

      -- lookup assertions
      lookupArgs <- withSrArgsM ["lookup", "*"]
      lookupResult <- liftIO $ captureSafeRm lookupArgs
      expectedLookup <-
        mkLookupDirSize ["df1", "df2", "df3"] [("ddir1", Nothing), ("dir2", Just "15.00B")]
      assertMatches expectedLookup lookupResult
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

pathSrc :: OsPath
pathSrc = [osp|src|]

pathDest :: OsPath
pathDest = [osp|dest|]
