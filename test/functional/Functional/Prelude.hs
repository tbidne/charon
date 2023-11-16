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

    -- ** Runners
    FuncEnv.runSafeRm,
    FuncEnv.runSafeRmException,
    FuncEnv.runIndexMetadataM,
    FuncEnv.runIndexMetadataTestDirM,

    -- ** Data capture
    FuncEnv.captureSafeRm,
    FuncEnv.captureSafeRmLogs,
    FuncEnv.captureSafeRmException,
    FuncEnv.captureSafeRmExceptionLogs,

    -- * Assertions
    assertPathsExist,
    assertPathsDoNotExist,
    assertSetEq,

    -- * Lookup
    mkLookupSimple,
    mkLookupFileOpath,
    mkLookupDirSize,
    mkLookupFull,
    mkLookupFullPath,

    -- * Misc
    withSrArgsM,
    withSrArgsPathsM,
    withSrArgsTestDirM,
    FuncEnv.mkPathDataSetM,
    FuncEnv.mkPathDataSetM2,
    FuncEnv.mkPathDataSetTestDirM,
    appendTestDir,
    appendTestDirM,
    FuncEnv.getTestDir,
    (</>!),
    foldFilePaths,
    foldFilePathsAcc,
    cfp,
  )
where

import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.FileSystem.Utils
  ( unsafeDecodeOsToFp,
    unsafeEncodeFpToOs,
    (</>!),
  )
import Effects.FileSystem.Utils qualified as FsUtils
import Functional.Prelude.FuncEnv (TestEnv, TestM)
import Functional.Prelude.FuncEnv qualified as FuncEnv
import Numeric.Literal.Integer as X (FromInteger (afromInteger))
import SafeRm.Backend.Data qualified as Backend
import SafeRm.Data.PathType as X (PathType (PathTypeDirectory, PathTypeFile))
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
(@=?) :: (Eq a, HasCallStack, MonadIO m, Show a) => a -> a -> m ()
x @=? y = liftIO $ x HUnit.@=? y

infix 1 @=?

-- | Assert paths exist.
assertPathsExist :: (MonadIO m) => [OsPath] -> m ()
assertPathsExist paths = liftIO
  $ for_ paths
  $ \p -> do
    exists <- doesPathExist p
    assertBool ("Expected path to exist: " <> show p) exists

-- | Asserts that paths do not exist.
assertPathsDoNotExist :: (MonadIO m) => [OsPath] -> m ()
assertPathsDoNotExist paths = liftIO
  $ for_ paths
  $ \p -> do
    exists <- doesPathExist p
    assertBool ("Expected path not to exist: " <> show p) (not exists)

mkLookupSimple :: [Text] -> [Text] -> TestM [TextMatch]
mkLookupSimple files dirs =
  mkLookupFull ((\n -> (n, n)) <$> files) ((,Nothing) <$> dirs)

mkLookupFileOpath :: [(Text, Text)] -> [Text] -> TestM [TextMatch]
mkLookupFileOpath files dirs =
  mkLookupFull files ((,Nothing) <$> dirs)

mkLookupDirSize :: [Text] -> [(Text, Maybe Text)] -> TestM [TextMatch]
mkLookupDirSize files = mkLookupFull ((\n -> (n, n)) <$> files)

mkLookupFull :: [(Text, Text)] -> [(Text, Maybe Text)] -> TestM [TextMatch]
mkLookupFull files dirs = do
  testDir <- asks (view #testDir)
  mkLookupFullPath testDir files dirs

mkLookupFullPath :: OsPath -> [(Text, Text)] -> [(Text, Maybe Text)] -> TestM [TextMatch]
mkLookupFullPath testDir files dirs = do
  testDirTxt <- T.pack <$> decodeOsToFpThrowM testDir

  let fromFile :: (Text, Text) -> [TextMatch]
      fromFile (n, o) =
        [ Exact $ "Name:      " <> n,
          Outfixes "Original:  " ["/safe-rm/functional/", testDirTxt] o,
          Exact "Type:      File",
          Exact "Size:      5.00B",
          Exact "Created:   2020-05-31 12:00:00"
        ]
      fromDir :: (Text, Maybe Text) -> [TextMatch]
      fromDir (d, sz) =
        [ Exact $ "Name:      " <> d,
          Outfixes "Original:  " ["/safe-rm/functional/", testDirTxt] d,
          Exact "Type:      Directory",
          Exact $ "Size:      " <> fromMaybe "5.00B" sz,
          Exact "Created:   2020-05-31 12:00:00"
        ]

      -- manual recursion so we can ensure files and dirs are interleaved
      -- correctly in sorted order
      foldPath [] [] = []
      foldPath (f : fs) [] = fromFile f : foldPath fs []
      foldPath [] (d : ds) = fromDir d : foldPath [] ds
      foldPath fss@(f@(fName, _) : fs) dss@(d@(dName, _) : ds)
        | fName <= dName = fromFile f : foldPath fs dss
        | otherwise = fromDir d : foldPath fss ds

      allMatches = foldPath filesSorted dirsSorted

  -- add newline between entries
  pure $ L.intercalate [Exact ""] allMatches
  where
    filesSorted = L.sortOn (\(f, _) -> T.toLower f) files
    dirsSorted = L.sortOn (\(d, _) -> T.toLower d) dirs

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
