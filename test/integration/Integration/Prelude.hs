{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Prelude for integration test suite.
module Integration.Prelude
  ( module X,

    -- * Assertions
    assertPathsExist,
    assertPathsDoNotExist,

    -- * Running SafeRm
    captureSafeRmIntExceptionPure,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effects.FileSystem.HandleWriter (MonadHandleWriter (hSetBuffering))
import Effects.System.Terminal (Window (Window))
import Effects.System.Terminal qualified as Term
import Effects.Time (MonadTime (getMonotonicTime, getSystemZonedTime))
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
import SafeRm.Backend.Data (Backend (BackendCbor))
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasBackend, HasTrashHome)
import SafeRm.Prelude as X
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Environment qualified as SysEnv
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.HUnit as X (testCase, (@=?))
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import Test.Utils as X

-- | Asserts that files exist.
assertPathsExist :: (Foldable f, MonadIO m, MonadTest m) => f OsPath -> m ()
assertPathsExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ doesFileExist p
    annotate $ "Expected file to exist: " <> show p
    assert exists

-- | Asserts that files do not exist.
assertPathsDoNotExist :: (Foldable f, MonadIO m, MonadTest m) => f OsPath -> m ()
assertPathsDoNotExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ doesFileExist p
    annotate $ "Expected file not to exist: " <> show p
    assert (not exists)

-- | Environment for running pure integration tests.
data IntPureEnv = MkIntPureEnv
  { backend :: Backend,
    trashHome :: PathI TrashHome,
    terminalRef :: IORef Text,
    deletedPathsRef :: !(IORef [OsPath])
  }

makeFieldLabelsNoPrefix ''IntPureEnv

deriving anyclass instance HasBackend IntPureEnv

deriving anyclass instance HasTrashHome IntPureEnv

-- NOTE: It would be nice to replace the IO with Identity, so we could be
-- really sure that we are not doing any IO. Alas, while we could mock
-- e.g. MonadPathReader (and possibly replace MonadIORef w/ STRef), we rely
-- on throwing exceptions, which requires IO.

-- | Type for running integration tests via a pure file-system. As this is
-- used to test protecting against root, it is VERY important that it cannot
-- actually write to the file system!!!
newtype IntPure env a = MkIntPure (ReaderT env IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIORef,
      MonadMask,
      MonadPathReader,
      MonadPosixCompat,
      MonadThread,
      MonadThrow,
      MonadReader env
    )
    via (ReaderT env IO)

instance MonadTerminal (IntPure IntPureEnv) where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  getChar = pure 'y'
  getTerminalSize =
    pure
      $ Window
        { height = 50,
          width = 100
        }

instance MonadTime (IntPure env) where
  getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getMonotonicTime = pure 0

instance MonadLogger (IntPure IntPureEnv) where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNS (IntPure IntPureEnv) where
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
      modifyIORef' ref (p1 :)

  renameDirectory p1 _ =
    asks (view #deletedPathsRef) >>= \ref ->
      modifyIORef' ref (p1 :)

  createDirectoryIfMissing _ _ = pure ()

runIntPure :: (IntPure env) a -> env -> IO a
runIntPure (MkIntPure rdr) = runReaderT rdr

-- | Runs safe-rm and captures a thrown exception, terminal, and
-- deleted paths.
captureSafeRmIntExceptionPure ::
  forall e.
  (Exception e) =>
  -- Args.
  [String] ->
  IO (Text, Text, Text)
captureSafeRmIntExceptionPure argList = do
  terminalRef <- newIORef ""
  deletedPathsRef <- newIORef []

  (toml, cmd) <- getConfig
  env <- mkIntPureEnv toml terminalRef deletedPathsRef

  result <- tryCS @_ @e $ runIntPure (Runner.runCmd cmd) env

  case result of
    Right _ ->
      error
        "captureSafeRmExceptionLogs: Expected exception, received none"
    Left ex -> do
      terminal <- readIORef terminalRef
      deletedPaths <- readIORef deletedPathsRef

      pure
        ( T.pack (displayException ex),
          terminal,
          showt deletedPaths
        )
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

mkIntPureEnv :: TomlConfig -> IORef Text -> IORef [OsPath] -> IO IntPureEnv
mkIntPureEnv toml terminalRef deletedPathsRef = do
  trashHome <- getTrashHome'
  pure
    $ MkIntPureEnv
      { backend = fromMaybe BackendCbor (toml ^. #backend),
        trashHome = trashHome,
        terminalRef,
        deletedPathsRef
      }
  where
    getTrashHome' = case toml ^. #trashHome of
      Nothing -> error "Setup error, no trash home on config"
      Just th -> pure th
