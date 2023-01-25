{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Prelude for integration test suite.
--
-- @since 0.1
module Integration.Prelude
  ( module X,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,

    -- * Running SafeRm
    captureSafeRmIntPure,
    captureSafeRmIntExceptionPure,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (..))
import Data.Time.LocalTime (midday, utc)
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.FileSystem.PathSize qualified as PathSize
import Effects.System.Terminal (MonadTerminal (..), Window (..))
import Effects.Time (MonadTime (..))
import Hedgehog as X
  ( Gen,
    MonadGen,
    MonadTest,
    Property,
    annotate,
    annotateShow,
    assert,
    failure,
    forAll,
    property,
    withTests,
    (===),
  )
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome)
import SafeRm.FileUtils as X
import SafeRm.Prelude as X
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Environment qualified as SysEnv
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.Golden as X (goldenVsString, goldenVsStringDiff)
import Test.Tasty.Hedgehog as X (testPropertyNamed)

-- | Asserts that files exist.
--
-- @since 0.1
assertFilesExist :: (Foldable f, MonadIO m, MonadTest m) => f FilePath -> m ()
assertFilesExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ doesFileExist p
    annotate $ "Expected file to exist: " <> p
    assert exists

-- | Asserts that files do not exist.
--
-- @since 0.1
assertFilesDoNotExist :: (Foldable f, MonadIO m, MonadTest m) => f FilePath -> m ()
assertFilesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ doesFileExist p
    annotate $ "Expected file not to exist: " <> p
    assert (not exists)

-- | Environment for running pure integration tests.
data IntPureEnv = MkIntPureEnv
  { trashHome :: !(PathI TrashHome),
    terminalRef :: !(IORef Text),
    deletedPathsRef :: !(IORef String)
  }

makeFieldLabelsNoPrefix ''IntPureEnv

deriving anyclass instance HasTrashHome IntPureEnv

-- | Type for running integration tests via a pure file-system. As this is
-- used to test protecting against root, it is VERY important that it cannot
-- actually write to the file system!!!
newtype IntPure env a = MkIntPure (ReaderT env IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadExit,
      MonadIORef,
      MonadPathReader,
      MonadThrow,
      MonadReader env
    )
    via (ReaderT env IO)

instance MonadPathSize (IntPure env) where
  findLargestPaths _ _ =
    pure $
      PathSize.PathSizeSuccess $
        PathSize.MkSubPathData $
          NESeq.singleton $
            PathSize.MkPathData
              { PathSize.path = "",
                PathSize.size = 5,
                PathSize.numFiles = 10,
                PathSize.numDirectories = 20
              }

instance MonadTerminal (IntPure IntPureEnv) where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  getChar = pure 'y'
  getTerminalSize =
    pure $
      Window
        { height = 50,
          width = 100
        }

instance MonadTime (IntPure env) where
  getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getMonotonicTime = pure 0

instance MonadLogger (IntPure IntPureEnv) where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNamespace (IntPure IntPureEnv) where
  getNamespace = pure ""
  localNamespace _ = id

instance MonadFileReader (IntPure IntPureEnv) where
  readBinaryFile _ = error "oh no"

-- NOTE: No real file-system operations!!!
instance MonadFileWriter (IntPure IntPureEnv) where
  writeBinaryFile _ _ = pure ()

-- NOTE: No real file-system operations!!!
instance MonadHandleWriter (IntPure IntPureEnv) where
  hSetBuffering _ _ = pure ()

-- NOTE: Intentionally unimplemented. We do not want this class to actually
-- have the ability to write/delete files!!!
instance MonadPathWriter (IntPure IntPureEnv) where
  renameFile p1 _ =
    asks (view #deletedPathsRef) >>= \ref ->
      modifyIORef' ref (\rest -> p1 <> "\n" <> rest)

  renameDirectory p1 _ =
    asks (view #deletedPathsRef) >>= \ref ->
      modifyIORef' ref (\rest -> p1 <> "\n" <> rest)

  createDirectoryIfMissing _ _ = pure ()

runIntPure :: (IntPure env) a -> env -> IO a
runIntPure (MkIntPure rdr) = runReaderT rdr

-- | Runs safe-rm and captures (terminal output, deleted paths).
captureSafeRmIntPure ::
  -- | Title to add to captured output.
  Builder ->
  -- Args.
  [String] ->
  IO (CapturedOutput, CapturedOutput)
captureSafeRmIntPure title argList = do
  terminalRef <- newIORef ""
  deletedPathsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkIntPureEnv toml terminalRef deletedPathsRef

  runIntPure (Runner.runCmd cmd) env

  terminal <- readIORef terminalRef
  deletedPaths <- readIORef deletedPathsRef

  let terminalBs = Builder.byteString $ encodeUtf8 terminal
      deletedBs = strToBuilder deletedPaths
  pure (MonadTerminal title terminalBs, DeletedPaths title deletedBs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs safe-rm and captures a thrown exception, terminal, and
-- deleted paths.
captureSafeRmIntExceptionPure ::
  forall e.
  Exception e =>
  -- | Title to add to captured output.
  Builder ->
  -- Args.
  [String] ->
  IO (CapturedOutput, CapturedOutput, CapturedOutput)
captureSafeRmIntExceptionPure title argList = do
  terminalRef <- newIORef ""
  deletedPathsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkIntPureEnv toml terminalRef deletedPathsRef

  result <- tryWithCallStack @e $ runIntPure (Runner.runCmd cmd) env

  case result of
    Right _ ->
      error
        "captureSafeRmExceptionLogs: Expected exception, received none"
    Left ex -> do
      terminal <- readIORef terminalRef
      deletedPaths <- readIORef deletedPathsRef

      let terminalBs = Builder.byteString $ encodeUtf8 terminal
          exceptionBs = exToBuilder Nothing ex
          deletedBs = strToBuilder deletedPaths
      pure
        ( Exception title exceptionBs,
          MonadTerminal title terminalBs,
          DeletedPaths title deletedBs
        )
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

mkIntPureEnv :: TomlConfig -> IORef Text -> IORef String -> IO IntPureEnv
mkIntPureEnv toml terminalRef deletedPathsRef = do
  trashHome <- getTrashHome'
  pure $
    MkIntPureEnv
      { trashHome = trashHome,
        terminalRef,
        deletedPathsRef
      }
  where
    getTrashHome' = case toml ^. #trashHome of
      Nothing -> error "Setup error, no trash home on config"
      Just th -> pure th
