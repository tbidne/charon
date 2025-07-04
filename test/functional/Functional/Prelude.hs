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
    FuncEnv.runCharonException,
    FuncEnv.runIndexMetadataM,
    FuncEnv.runIndexMetadataTestDirM,

    -- ** Data capture
    FuncEnv.captureCharon,
    FuncEnv.captureCharonEnv,
    FuncEnv.captureCharonLogs,
    FuncEnv.captureCharonEnvLogs,
    FuncEnv.captureCharonException,
    FuncEnv.captureCharonExceptionLogs,
    FuncEnv.captureCharonExceptionTerminal,

    -- *** ByteString
    captureIndex,
    captureMetadata,
    captureCharonExceptionTermBS,

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
    concatBs,
    terminalToBs,
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
import Data.ByteString qualified as BS
import Data.Char qualified as Ch
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

foldFilePaths :: [FilePath] -> FilePath
foldFilePaths = foldFilePathsAcc ""

foldFilePathsAcc :: FilePath -> [FilePath] -> FilePath
foldFilePathsAcc = foldl' cfp

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths

{- ORMOLU_DISABLE -}

-- See NOTE: [Windows getFileSize]
mkMetadata :: Natural -> Natural -> Integer -> Integer -> Metadata
mkMetadata numEntries numFiles _logSize _size =
  MkMetadata
    { numEntries,
      numFiles,
      logSize = fromℤ 0,
      size = fromℤ 0
    }

{- ORMOLU_ENABLE -}

data GoldenParams = MkGoldenParams
  { runner :: IO ByteString,
    -- | Test string description
    testDesc :: TestName,
    -- | Test function name, for creating unique file paths.
    testName :: OsPath
  }

makeFieldLabelsNoPrefix ''GoldenParams

testGoldenParamsOs :: GoldenParams -> TestTree
testGoldenParamsOs = testGoldenParams' True

testGoldenParams :: GoldenParams -> TestTree
testGoldenParams = testGoldenParams' False

testGoldenParams' :: Bool -> GoldenParams -> TestTree
testGoldenParams' diffOs params = goldenDiffCustom desc goldenPath actualPath $ do
  bs <- params ^. #runner
  writeActualFile bs
  where
    desc = params ^. #testDesc

    osSfx
      | diffOs = osExt
      | otherwise = [osstr||]

    goldenPath = unsafeDecode $ basePath <> osSfx <> [osstr|.golden|]
    actualPath = unsafeDecode $ basePath <> osSfx <> [osstr|.actual|]

    writeActualFile :: ByteString -> IO ()
    writeActualFile =
      writeBinaryFileIO (unsafeEncode actualPath)
        . (<> "\n")

    basePath = [ospPathSep|test/functional/goldens|] </> (params ^. #testName)

terminalToBs :: OsPath -> [Text] -> ByteString
terminalToBs testDir = terminalToBs' replaceFn
  where
    testDirTxt = T.pack $ unsafeDecode testDir

    replaceFn =
      T.replace "\\" "/"
        . T.replace testDirTxt "<dir>"

terminalToBs' :: (Text -> Text) -> [Text] -> ByteString
terminalToBs' modTxt =
  encodeUtf8
    . T.strip
    . T.unlines
    . fmap modTxt

concatBs :: ByteString -> ByteString -> ByteString
concatBs x y = strip $ x <> "\n\n" <> y
  where
    strip = BS.dropWhile isSpc . BS.dropWhileEnd isSpc
    isSpc = Ch.isSpace . Ch.chr . fromIntegral

captureIndex :: OsPath -> TestM ByteString
captureIndex testDir = do
  indexArgs <- withSrArgsM ["list", "--format", "s"]
  indexTxt <- FuncEnv.captureCharon indexArgs

  pure $ terminalToBs testDir indexTxt

captureMetadata :: TestM ByteString
captureMetadata = do
  metadataArgs <- withSrArgsM ["metadata"]
  metadataTxt <- FuncEnv.captureCharon metadataArgs
  pure $ terminalToBs' id metadataTxt

captureCharonExceptionTermBS ::
  forall e.
  (Exception e) =>
  OsPath ->
  [String] ->
  TestM ByteString
captureCharonExceptionTermBS testDir argList = do
  (ex, term) <- liftIO $ FuncEnv.captureCharonExceptionTerminal @e argList
  pure $ terminalToBs testDir $ ex : "" : term

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
