{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for functional test suite.
module Functional.Prelude
  ( module X,

    -- * Lifted HUnit
    (@=?),

    -- * Running Charon

    -- ** Test Environment
    TestM,
    TestEnv (..),

    -- ** Runners
    FuncEnv.runCharon,
    FuncEnv.runCharonException,
    FuncEnv.runIndexMetadataM,
    FuncEnv.runIndexMetadataTestDirM,

    -- ** Data capture
    FuncEnv.captureCharon,
    FuncEnv.captureCharonLogs,
    FuncEnv.captureCharonException,
    FuncEnv.captureCharonExceptionLogs,

    -- * Assertions
    assertPathsExist,
    assertSymlinksExist,
    assertPathsDoNotExist,
    assertSymlinksDoNotExist,
    assertSetEq,
    FuncEnv.assertFdoDirectorySizesM,
    FuncEnv.assertFdoDirectorySizesTestDirM,
    FuncEnv.assertFdoDirectorySizesArgsM,
    FuncEnv.assertFdoDirectorySizesArgsNoOrderM,

    -- * Misc
    withSrArgsM,
    withSrArgsPathsM,
    withSrArgsTestDirM,
    FuncEnv.mkPathDataSetM,
    FuncEnv.mkPathDataSetM2,
    FuncEnv.mkPathDataSetTestDirM,
    mkMetadata,
    appendTestDir,
    appendTestDirM,
    FuncEnv.getTestDir,
    (</>!),
    foldFilePaths,
    foldFilePathsAcc,
    cfp,
  )
where

import Charon.Backend.Data qualified as Backend
import Charon.Data.Metadata
  ( Metadata
      ( MkMetadata,
        logSize,
        numEntries,
        numFiles,
        size
      ),
  )
import Charon.Data.PathType as X (PathTypeW (MkPathTypeW))
import Charon.Prelude as X
import Data.HashSet qualified as HSet
import Data.Text.Lazy qualified as TL
import Effects.FileSystem.Utils
  ( unsafeDecodeOsToFp,
    unsafeEncodeFpToOs,
    (</>!),
  )
import Effects.FileSystem.Utils qualified as FsUtils
import Functional.Prelude.FuncEnv (TestEnv, TestM, (@=?))
import Functional.Prelude.FuncEnv qualified as FuncEnv
import Numeric.Literal.Integer as X (FromInteger (afromInteger))
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Utils as X
import Text.Pretty.Simple qualified as Pretty

-- | Assert paths exist.
assertPathsExist :: (MonadIO m) => [OsPath] -> m ()
assertPathsExist paths = liftIO
  $ for_ paths
  $ \p -> do
    exists <- doesPathExist p
    assertBool ("Expected path to exist: " <> show p) exists

assertSymlinksExist :: (MonadIO m) => [OsPath] -> m ()
assertSymlinksExist paths = liftIO
  $ for_ paths
  $ \p -> do
    exists <- doesSymbolicLinkExist p
    assertBool ("Expected symlink to exist: " <> show p) exists

-- | Asserts that paths do not exist.
assertPathsDoNotExist :: (MonadIO m) => [OsPath] -> m ()
assertPathsDoNotExist paths = liftIO
  $ for_ paths
  $ \p -> do
    exists <- doesPathExist p
    assertBool ("Expected path not to exist: " <> show p) (not exists)

-- | Asserts that paths do not exist.
assertSymlinksDoNotExist :: (MonadIO m) => [OsPath] -> m ()
assertSymlinksDoNotExist paths = liftIO
  $ for_ paths
  $ \p -> do
    exists <- doesSymbolicLinkExist p
    assertBool ("Expected path symlink to exist: " <> show p) (not exists)

assertSetEq :: (Hashable a, MonadIO m, Show a) => HashSet a -> HashSet a -> m ()
assertSetEq x y = do
  unless (HSet.null xdiff)
    $ liftIO
    $ assertFailure
    $ TL.unpack (prettySet "Expected" "Results" xdiff y)

  unless (HSet.null ydiff)
    $ liftIO
    $ assertFailure
    $ TL.unpack (prettySet "Results" "Expected" ydiff x)
  where
    xdiff = HSet.difference x y
    ydiff = HSet.difference y x

    prettySet d e s t =
      mconcat
        [ d,
          " contained elements not found in ",
          e,
          ":\n",
          p' s,
          "\n",
          e,
          ":\n",
          p' t
        ]

    p' = HSet.foldl' (\acc z -> Pretty.pShow z <> "\n" <> acc) ""

-- | Prepends the given arguments with the trash directory and backend,
-- according to the environment i.e.
--
-- @trashDir == <testRoot>\/<testDir>-<backend>\/<trashDir>@
withSrArgsM :: [String] -> TestM [String]
withSrArgsM as = do
  testDir <- FuncEnv.getTestDir
  withSrArgsTestDirM testDir as

-- | Differs from 'withSrArgsM' in that we receive the literal testDir,
-- rather than grabbing it from the env.
withSrArgsTestDirM :: OsPath -> [String] -> TestM [String]
withSrArgsTestDirM testDir as = do
  env <- ask

  let backend = env ^. #backend
      trashDir = testDir </> (env ^. #trashDir)
  pure $ ["-t", unsafeDecodeOsToFp trashDir, "-b", Backend.backendName backend] ++ as

-- | Prepends the given arguments with the trash directory and backend,
-- according to the environment i.e.
--
-- @trashDir == <testRoot>\/<testDir>-<backend>\/<trashDir>@
withSrArgsPathsM :: [String] -> [OsPath] -> TestM [String]
withSrArgsPathsM as paths = withSrArgsM (as ++ (unsafeDecodeOsToFp <$> paths))

-- | Appends the given string to the testDir and creates the current full
-- testDir according to 'FuncEnv.getTestDir'.
appendTestDirM :: String -> TestM a -> TestM a
appendTestDirM d m = local (appendTestDir d) $ do
  testDir <- FuncEnv.getTestDir
  liftIO . clearDirectory $ testDir
  m

-- | Appends to the testDir.
appendTestDir :: String -> TestEnv -> TestEnv
appendTestDir d = over' #testDir (</> unsafeEncodeFpToOs d)

foldFilePaths :: [FilePath] -> FilePath
foldFilePaths = foldFilePathsAcc ""

foldFilePathsAcc :: FilePath -> [FilePath] -> FilePath
foldFilePathsAcc = foldl' cfp

cfp :: FilePath -> FilePath -> FilePath
cfp = FsUtils.combineFilePaths

{- ORMOLU_DISABLE -}

-- See NOTE: [Windows getFileSize]
mkMetadata :: Natural -> Natural -> Integer -> Integer -> Metadata
mkMetadata numEntries numFiles _logSize _size =
  MkMetadata
    { numEntries,
      numFiles,
#if WINDOWS
      logSize = afromInteger 0,
      size = afromInteger 0
#else
      logSize = afromInteger _logSize,
      size = afromInteger _size
#endif
    }

{- ORMOLU_ENABLE -}
