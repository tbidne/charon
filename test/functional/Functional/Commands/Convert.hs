{-# LANGUAGE QuasiQuotes #-}

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

    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
        fileLinkToDelete = testDir </> [osp|file-link|]
        dirLinkToDelete = testDir </> [osp|dir-link|]
        linksToDelete = [fileLinkToDelete, dirLinkToDelete]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

    -- setup
    clearDirectory testDir
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
    -- test w/ a file in dir
    createFiles ([testDir </>! "dir2/dir3/foo"] <> filesToDelete)
    createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
    assertPathsExist (filesToDelete ++ dirsToDelete)
    assertSymlinksExist linksToDelete

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
    assertSymlinksDoNotExist linksToDelete

    -- trash structure assertions
    delExpectedIdxSet <-
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

      -- trash structure assertions
      convertExpectedIdxSet <-
        -- See Note [Test dir and backend changes].
        mkPathDataSetTestDirM
          testDir
          [ ("f1", PathTypeFile, 5),
            ("f2", PathTypeFile, 5),
            ("f3", PathTypeFile, 5),
            ("dir1", PathTypeDirectory, 5),
            ("dir2", PathTypeDirectory, 15),
            ("dir4", PathTypeDirectory, 10),
            ("dir-link", PathTypeSymlink, 5),
            ("file-link", PathTypeSymlink, 5)
          ]

      -- See Note [Test dir and backend changes].
      (convertIdxSet, convertMetadata) <- runIndexMetadataTestDirM testDir
      assertSetEq convertExpectedIdxSet convertIdxSet
      delExpectedMetadata @=? convertMetadata
  where
    destDesc = Backend.Data.backendName dest

    delExpectedMetadata =
      MkMetadata
        { numEntries = 8,
          numFiles = 7,
          logSize = afromInteger 0,
          size = afromInteger 55
        }
