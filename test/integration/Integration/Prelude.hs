{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effectful.FileSystem.FileReader.Dynamic
  ( FileReaderDynamic (ReadBinaryFile),
  )
import Effectful.FileSystem.FileReader.Dynamic qualified as FR
import Effectful.FileSystem.FileWriter.Dynamic
  ( FileWriterDynamic (WriteBinaryFile),
  )
import Effectful.FileSystem.HandleWriter.Dynamic
  ( HandleWriterDynamic (HSetBuffering),
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic
  ( PathWriterDynamic
      ( CreateDirectoryIfMissing,
        RenameDirectory,
        RenameFile
      ),
  )
import Effectful.Logger.Dynamic (LoggerDynamic (LoggerLog))
import Effectful.LoggerNS.Dynamic
  ( LoggerNSDynamic
      ( GetNamespace,
        LocalNamespace
      ),
  )
import Effectful.Terminal.Dynamic
  ( TerminalDynamic
      ( GetChar,
        GetContents',
        GetLine,
        GetTerminalSize,
        PutBinary,
        PutStr,
        PutStrLn
      ),
    Window (Window, height, width),
  )
import Effectful.Time.Dynamic
  ( TimeDynamic
      ( GetMonotonicTime,
        GetSystemZonedTime
      ),
  )
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
import SafeRm.Data.Backend (Backend (BackendCbor))
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
  { backend :: !Backend,
    trashHome :: !(PathI TrashHome),
    terminalRef :: !(IORef Text),
    deletedPathsRef :: !(IORef [OsPath])
  }

makeFieldLabelsNoPrefix ''IntPureEnv

deriving anyclass instance HasBackend IntPureEnv

deriving anyclass instance HasTrashHome IntPureEnv

runTerminal ::
  ( IORefStatic :> es,
    Reader IntPureEnv :> es
  ) =>
  Eff (TerminalDynamic : es) a ->
  Eff es a
runTerminal = interpret $ \_ -> \case
  PutStr s ->
    asks @IntPureEnv (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  GetChar -> pure 'y'
  GetTerminalSize ->
    pure
      $ Window
        { height = 50,
          width = 100
        }
  PutStrLn s ->
    asks @IntPureEnv (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack (s <> "\n"))
  PutBinary _ -> error "PutBinary: unimplemented"
  GetLine -> error "GetLine: unimplemented"
  GetContents' -> error "GetContents': unimplemented"

runTime :: Eff (TimeDynamic : es) a -> Eff es a
runTime = interpret $ \_ -> \case
  GetSystemZonedTime -> pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  GetMonotonicTime -> pure 0

runLogger :: Eff (LoggerDynamic : es) a -> Eff es a
runLogger = interpret $ \_ -> \case
  LoggerLog {} -> pure ()

runLoggerNS :: Eff (LoggerNSDynamic : es) a -> Eff es a
runLoggerNS = interpret $ \env -> \case
  GetNamespace -> pure ""
  LocalNamespace _ m -> localSeqUnlift env $ \runner -> runner m

runFileReader :: Eff (FileReaderDynamic : es) a -> Eff es a
runFileReader = interpret $ \_ -> \case
  ReadBinaryFile _ -> error "oh no"

-- NOTE: No real file-system operations!!!
runFileWriter :: Eff (FileWriterDynamic : es) a -> Eff es a
runFileWriter = interpret $ \_ -> \case
  WriteBinaryFile {} -> pure ()
  _ -> error "runFileWriter: unimplemented"

-- NOTE: No real file-system operations!!!
runHandleWriter :: Eff (HandleWriterDynamic : es) a -> Eff es a
runHandleWriter = interpret $ \_ -> \case
  HSetBuffering {} -> pure ()
  _ -> error "runHandleWriter: unimplemented"

-- NOTE: Intentionally unimplemented. We do not want this class to actually
-- have the ability to write/delete files!!!
runPathWriter ::
  ( IORefStatic :> es,
    Reader IntPureEnv :> es
  ) =>
  Eff (PathWriterDynamic : es) a ->
  Eff es a
runPathWriter = interpret $ \_ -> \case
  RenameFile p1 _ ->
    asks @IntPureEnv (view #deletedPathsRef) >>= \ref -> modifyIORef' ref (p1 :)
  RenameDirectory p1 _ ->
    asks @IntPureEnv (view #deletedPathsRef) >>= \ref -> modifyIORef' ref (p1 :)
  CreateDirectoryIfMissing _ _ -> pure ()
  _ -> error "runPathWriter: unimplemented"

runIntPure ::
  Eff
    [ TimeDynamic,
      TerminalDynamic,
      PosixCompatStatic,
      PathWriterDynamic,
      PathReaderDynamic,
      LoggerNSDynamic,
      LoggerDynamic,
      IORefStatic,
      HandleWriterDynamic,
      FileWriterDynamic,
      FileReaderDynamic,
      Reader IntPureEnv,
      Concurrent,
      IOE
    ]
    a ->
  IntPureEnv ->
  IO a
runIntPure x env =
  runEff
    . runConcurrent
    . runReader env
    . runFileReader
    . runFileWriter
    . runHandleWriter
    . runIORefStaticIO
    . runLogger
    . runLoggerNS
    . PR.runPathReaderDynamicIO
    . runPathWriter
    . runPosixCompatStaticIO
    . runTerminal
    . runTime
    $ x

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

  result <- try @_ @e $ runIntPure (Runner.runCmd @IntPureEnv cmd) env

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
    getConfig = SysEnv.withArgs argList' (run Runner.getConfiguration)

    run =
      runEff
        . FR.runFileReaderDynamicIO
        . runOptparseStaticIO
        . PR.runPathReaderDynamicIO

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
