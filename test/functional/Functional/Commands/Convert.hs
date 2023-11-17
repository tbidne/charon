-- | Tests for convert command.
module Functional.Commands.Convert
  ( tests,
  )
where

import Functional.Prelude
import SafeRm.Backend.Data (Backend)
import SafeRm.Backend.Data qualified as Backend.Data
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
    "Convert Command"
    ( (`convertsBackend` testEnv') <$> [minBound .. maxBound]
    )
  where
    testEnv' = appendTestDir "convert" <$> testEnv

convertsBackend :: Backend -> IO TestEnv -> TestTree
convertsBackend dest getTestEnv = testCase ("Converts backend to " ++ destDesc) $ do
  testEnv <- getTestEnv
  let testDirPath =
        mconcat
          [ "convertsBackend-",
            destDesc
          ]

  usingReaderT testEnv $ appendTestDirM testDirPath $ do
    testDir <- getTestDir
    relativeTestDir <- asks (view #testDir)

    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

    -- setup
    clearDirectory testDir
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ([testDir </>! "dir2/dir3/foo"] <> filesToDelete)
    assertPathsExist (filesToDelete ++ dirsToDelete)

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "f1", "f2", "f3", "dir*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupDirSize ["f1", "f2", "f3"] [("dir1", Nothing), ("dir2", Just "15.00B")]
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- CONVERT

    convertArgList <- withSrArgsM ["convert", "-d", Backend.Data.backendName dest]
    runSafeRm convertArgList

    -- we changed the backend, so have to update our env here
    local (set' #backend dest) $ do
      -- same file assertions

      -- NOTE: [Test dir and backend changes]
      --
      -- We change the backend here as we want to test reading the trash
      -- files we just converted. This presents a problem: our usual helper
      -- functions e.g. mkAllTrashPathsM, mkPathDataSetM, runIndexMetadataTestDirM
      -- use the current backend when determining the current test directory
      -- i.e. they use getTestDir, which appends:
      --
      --      <> "-"
      --      <> Backend.Data.backendName (env ^. #backend)
      --
      -- onto the end of the path. This is normally what we want, but here
      -- it isn't, as it will calculate a new (wrong) trash directory.
      -- For instance testDir above may be
      --
      --    /tmp/safe-rm/functional/convert/convertsBackend-(dest=cbor)-cbor
      --
      -- but after this backend change, it will be
      --
      -- /tmp/safe-rm/functional/convert/convertsBackend-(dest=cbor)-fdo
      --
      -- This is not what we want, as the test dir has not changed! Thus we
      -- resort to variants of the aforementioned that explicitly take in
      -- the test dir: our previously calculated testDir.

      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- lookup assertions
      lookupArgs2 <- withSrArgsTestDirM testDir ["lookup", "f1", "f2", "f3", "dir*"]
      lookupResult2 <- liftIO $ captureSafeRm lookupArgs2
      expectedLookup2 <-
        mkLookupFullPath
          relativeTestDir
          [("f1", "f1"), ("f2", "f2"), ("f3", "f3")]
          [("dir1", Nothing), ("dir2", Just "15.00B")]
      assertMatches expectedLookup2 lookupResult2

      -- trash structure assertions
      convertExpectedIdxSet <-
        -- See Note [Test dir and backend changes].
        mkPathDataSetTestDirM
          testDir
          [ ("f1", PathTypeFile, 5),
            ("f2", PathTypeFile, 5),
            ("f3", PathTypeFile, 5),
            ("dir1", PathTypeDirectory, 5),
            ("dir2", PathTypeDirectory, 15)
          ]

      -- See Note [Test dir and backend changes].
      (convertIdxSet, convertMetadata) <- runIndexMetadataTestDirM testDir
      assertSetEq convertExpectedIdxSet convertIdxSet
      delExpectedMetadata @=? convertMetadata
  where
    destDesc = Backend.Data.backendName dest

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 35
        }
