{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for functional test suite.
module Functional.Prelude
  ( module X,

    -- * Lifted HUnit
    (@=?),

    -- * Running SafeRm

    -- ** Test Environment
    TestM,
    TestEnv (..),
    FuncEnv.usingTestM,

    -- ** Safe-rm Runners
    FuncEnv.runSafeRm,
    FuncEnv.runSafeRmException,
    FuncEnv.runIndexMetadataM,
    FuncEnv.runIndexMetadataTestDirM,

    -- *** Data capture
    FuncEnv.captureSafeRm,
    FuncEnv.captureSafeRmLogs,
    FuncEnv.captureSafeRmExceptionLogs,

    -- * Assertions
    assertPathsExist,
    assertPathsDoNotExist,
    assertSetEq,

    -- * Misc
    withSrArgsM,
    withSrArgsPathsM,
    FuncEnv.mkPathDataSetM,
    FuncEnv.mkPathDataSetM2,
    FuncEnv.mkPathDataSetTestDirM,
    appendTestDir,
    appendTestDirM,
    FuncEnv.getTestDir,
    mkAllTrashPathsM,
    (</>!),
    foldFilePaths,
    foldFilePathsAcc,
    cfp,
  )
where

import Data.HashSet qualified as HSet
import Data.Text.Lazy qualified as TL
import Effectful.FileSystem.PathReader.Static qualified as PRStatic
import Effectful.FileSystem.Utils
  ( unsafeDecodeOsToFp,
    unsafeEncodeFpToOs,
    (</>!),
  )
import Effectful.FileSystem.Utils qualified as FsUtils
import Functional.Prelude.FuncEnv (TestEnv, TestM)
import Functional.Prelude.FuncEnv qualified as FuncEnv
import Numeric.Literal.Integer as X (FromInteger (afromInteger))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Env qualified as Env
import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.HUnit qualified as HUnit
import Test.Utils as X
import Text.Pretty.Simple qualified as Pretty

-- | Lifted (@=?).
(@=?) :: (Eq a, HasCallStack, Show a) => a -> a -> TestM ()
x @=? y = liftIO $ x HUnit.@=? y

infix 1 @=?

-- | Assert paths exist.
assertPathsExist :: [OsPath] -> TestM ()
assertPathsExist paths =
  for_ paths
    $ \p -> liftIO $ do
      exists <- PRStatic.doesPathExist p
      assertBool ("Expected path to exist: " <> show p) exists

-- | Asserts that paths do not exist.
assertPathsDoNotExist :: [OsPath] -> TestM ()
assertPathsDoNotExist paths =
  for_ paths
    $ \p -> liftIO $ do
      exists <- PRStatic.doesPathExist p
      assertBool ("Expected path not to exist: " <> show p) (not exists)

-- | Transform each filepath @p@ to its files/ and info/ path, taking in
-- the env's trash root, test dir, trash dir, and backend.
mkAllTrashPathsM :: [FilePath] -> TestM [OsPath]
mkAllTrashPathsM paths = liftA2 (++) trashPaths trashInfoPaths
  where
    trashPaths = mkTrashPathsM (fmap unsafeEncodeFpToOs paths)
    trashInfoPaths = mkTrashInfoPathsM (fmap unsafeEncodeFpToOs paths)

-- | Transform each path @p@ to
-- @<testRoot>\/<testDir>-<backend>\/<trashDir>\/info\/p.<ext>@.
mkTrashInfoPathsM :: [OsPath] -> TestM [OsPath]
mkTrashInfoPathsM files = do
  env :: TestEnv <- ask
  testDir <- FuncEnv.getTestDir
  let ext = Env.trashInfoExtensionOsPath (env ^. #backend)
      mkTrashInfoPath p =
        testDir
          </> (env ^. #trashDir)
          </> pathInfo
          </> p
          <> ext
  pure $ fmap mkTrashInfoPath files

-- | Transform each path @p@ to
-- @<testRoot>\/<testDir>-<backend>\/<trashDir>\/files\/p@.
mkTrashPathsM :: [OsPath] -> TestM [OsPath]
mkTrashPathsM files = do
  env :: TestEnv <- ask
  testDir <- FuncEnv.getTestDir
  let mkTrashPath p =
        testDir
          </> (env ^. #trashDir)
          </> pathFiles
          </> p
  pure $ fmap mkTrashPath files

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
  env :: TestEnv <- ask

  testDir <- FuncEnv.getTestDir
  let backend = env ^. #backend
      trashDir = testDir </> (env ^. #trashDir)
  pure $ ["-t", unsafeDecodeOsToFp trashDir, "-b", Backend.backendArg backend] ++ as

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
