{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for functional test suite.
module Functional.Prelude
  ( module X,

    -- * Lifted HUnit
    (@=?),

    -- * Golden Tests
    GoldenParams (..),
    testGoldenParams,
    testGoldenParamsOs,

    -- * Running Charon

    -- ** Test Environment
    TestM,
    TestEnv (..),

    -- ** Runners
    FuncEnv.runCharon,
    FuncEnv.runCharonEnv,
    FuncEnv.runCharonE,

    -- ** Data capture
    FuncEnv.captureCharon,
    FuncEnv.captureCharonEnv,
    FuncEnv.captureCharonLogs,
    FuncEnv.captureCharonEnvLogs,
    FuncEnv.captureCharonLogsE,
    FuncEnv.captureCharonTermE,

    -- *** ByteString
    captureIndexBs,
    captureIndexTabularBs,
    captureIndexBackendBs,
    captureMetadataBs,
    captureCharonTermBsE,

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
    appendTestDir,
    appendTestDirM,
    FuncEnv.getTestDir,
    (</>!),
    cfp,
    terminalToBs,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend
import Charon.Data.PathType as X (PathTypeW (MkPathTypeW))
import Charon.Prelude as X
import Data.ByteString qualified as BS
import Data.HashSet qualified as HSet
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import FileSystem.IO (writeBinaryFileIO)
import FileSystem.OsPath
  ( combineFilePaths,
    unsafeDecode,
    unsafeEncode,
    unsafeEncodeValid,
    (</>!),
  )
import Functional.Prelude.FuncEnv (TestEnv, TestM, (@=?))
import Functional.Prelude.FuncEnv qualified as FuncEnv
import Test.Tasty as X (TestName, TestTree, testGroup)
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
withSrArgsM = withSrArgsEnvM id

withSrArgsEnvM :: (TestEnv -> TestEnv) -> [String] -> TestM [String]
withSrArgsEnvM modEnv as = do
  testDir <- FuncEnv.getTestDir
  withSrArgsTestDirEnvM modEnv testDir as

withSrArgsTestDirEnvM :: (TestEnv -> TestEnv) -> OsPath -> [String] -> TestM [String]
withSrArgsTestDirEnvM modEnv testDir as = do
  env <- modEnv <$> ask

  let backend = env ^. #backend
      trashDir = testDir </> (env ^. #trashDir)
  pure $ ["-t", unsafeDecode trashDir, "-b", Backend.backendName backend] ++ as

-- | Prepends the given arguments with the trash directory and backend,
-- according to the environment i.e.
--
-- @trashDir == <testRoot>\/<testDir>-<backend>\/<trashDir>@
withSrArgsPathsM :: [String] -> [OsPath] -> TestM [String]
withSrArgsPathsM as paths = withSrArgsM (as ++ (unsafeDecode <$> paths))

-- | Appends the given string to the testDir and creates the current full
-- testDir according to 'FuncEnv.getTestDir'.
appendTestDirM :: String -> TestM a -> TestM a
appendTestDirM d m = local (appendTestDir d) $ do
  testDir <- FuncEnv.getTestDir
  liftIO . clearDirectory $ testDir
  m

-- | Appends to the testDir.
appendTestDir :: String -> TestEnv -> TestEnv
appendTestDir d = over' #testDir (</> unsafeEncodeValid d)

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths

-- Test output, used to enforce output consistency.
data ByteStringRender
  = ByteStringOne ByteString
  | ByteStringMany [ByteString]

instance Semigroup ByteStringRender where
  x <> ByteStringMany [] = x
  ByteStringMany [] <> y = y
  x <> y = ByteStringMany (renderToList x <> renderToList y)

instance Monoid ByteStringRender where
  mempty = ByteStringMany []

renderToList :: ByteStringRender -> [ByteString]
renderToList (ByteStringOne x) = [x]
renderToList (ByteStringMany xs) = xs

-- | Separates each bytestring by a line of hyphens, for more easily
-- understanding the output.
renderBs :: ByteStringRender -> ByteString
renderBs =
  (<> hyphens)
    . BS.intercalate hyphens
    . renderToList
  where
    hyphens = BS.replicate 80 45 <> "\n"

data GoldenParams = MkGoldenParams
  { runner :: IO ByteStringRender,
    -- | Test string description
    testDesc :: TestName,
    -- | Test function name, for creating unique file paths.
    testName :: OsPath
  }

makeFieldLabelsNoPrefix ''GoldenParams

testGoldenParamsOs :: GoldenParams -> TestTree
testGoldenParamsOs = testGoldenParams' GoldenFileOsOn

testGoldenParams :: GoldenParams -> TestTree
testGoldenParams = testGoldenParams' GoldenFileOsOff

data GoldenFileOs
  = GoldenFileOsOff
  | GoldenFileOsOn

testGoldenParams' ::
  GoldenFileOs ->
  GoldenParams ->
  TestTree
testGoldenParams'
  fileOs
  params = goldenDiffCustom desc goldenPath actualPath $ do
    bs <- params ^. #runner
    writeActualFile $ renderBs bs
    where
      desc = params ^. #testDesc

      osSfx = case fileOs of
        GoldenFileOsOff -> [osstr||]
        GoldenFileOsOn -> osExt

      goldenPath = mkFile [osstr|.golden|]
      actualPath = mkFile [osstr|.actual|]

      mkFile =
        unsafeDecode
          . (basePath <>)
          . (osSfx <>)

      writeActualFile :: ByteString -> IO ()
      writeActualFile =
        writeBinaryFileIO (unsafeEncode actualPath)

      basePath = [ospPathSep|test/functional/goldens|] </> (params ^. #testName)

terminalToBs :: OsPath -> [Text] -> ByteStringRender
terminalToBs testDir = terminalToBs' replaceFn
  where
    testDirTxt = T.pack $ unsafeDecode testDir

    replaceFn =
      T.replace "\\" "/"
        . T.replace testDirTxt "<dir>"

terminalToBs' :: (Text -> Text) -> [Text] -> ByteStringRender
terminalToBs' modTxt =
  ByteStringOne
    . encodeUtf8
    . T.unlines
    . fmap modTxt

captureIndexBs :: OsPath -> TestM ByteStringRender
captureIndexBs testDir = do
  indexArgs <- withSrArgsM ["list", "--format", "single"]
  indexTxt <- FuncEnv.captureCharon indexArgs

  pure $ terminalToBs testDir indexTxt

captureIndexTabularBs :: OsPath -> TestM ByteStringRender
captureIndexTabularBs testDir = do
  -- t:     tabular over tabular-simple because tabular allows specifying lengths.
  -- -n 15: deterministic header underline length
  -- -o 3:  do not want any part of the original path, since it is
  --        non-deterministic (tmp dir).
  indexArgs <- withSrArgsM ["list", "--format", "tabular", "-n", "15", "-o", "3"]
  indexTxt <- FuncEnv.captureCharon indexArgs

  pure $ terminalToBs testDir indexTxt

captureIndexBackendBs :: Backend -> OsPath -> TestM ByteStringRender
captureIndexBackendBs backend testDir = do
  indexArgs <- withSrArgsEnvM (set' #backend backend) ["list", "--format", "single"]
  indexTxt <- FuncEnv.captureCharon indexArgs

  pure $ terminalToBs testDir indexTxt

captureMetadataBs :: TestM ByteStringRender
captureMetadataBs = do
  metadataArgs <- withSrArgsM ["metadata"]
  metadataTxt <- FuncEnv.captureCharon metadataArgs
  pure $ terminalToBs' id metadataTxt

captureCharonTermBsE ::
  forall e.
  (Exception e) =>
  OsPath ->
  [String] ->
  TestM ByteStringRender
captureCharonTermBsE testDir argList = do
  (ex, term) <- liftIO $ FuncEnv.captureCharonTermE @e argList
  let bs1 = terminalToBs testDir [ex]
      bs2 = terminalToBs testDir term
  pure $ bs1 <> bs2

{- ORMOLU_DISABLE -}

osExt :: OsString
osExt =
#if WINDOWS
  [osstr|_windows|]
#elif OSX
  [osstr|_osx|]
#else
  [osstr|_linux|]
#endif

{- ORMOLU_ENABLE -}
