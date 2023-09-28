{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.Prelude.FuncEnv
  ( -- * Types
    runFuncIO,
    FuncEnv (..),
    mkFuncEnv,

    -- * Running Test Environment
    TestM,
    TestEnv (..),
    usingTestM,

    -- ** Running Safe-rm
    runSafeRm,
    runSafeRmException,
    runIndexMetadataM,
    runIndexMetadataTestDirM,

    -- *** Data capture
    captureSafeRm,
    captureSafeRmLogs,
    captureSafeRmExceptionLogs,

    -- * Misc
    mkPathDataSetM,
    mkPathDataSetM2,
    mkPathDataSetTestDirM,
    getTestDir,
  )
where

import Data.HashSet qualified as HSet
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effectful.Environment qualified as EffEnv
import Effectful.FileSystem.FileReader.Dynamic (runFileReaderDynamicIO)
import Effectful.FileSystem.FileWriter.Dynamic (runFileWriterDynamicIO)
import Effectful.FileSystem.HandleWriter.Dynamic (runHandleWriterDynamicIO)
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic
      ( CanonicalizePath,
        DoesDirectoryExist,
        DoesFileExist,
        DoesPathExist,
        GetFileSize,
        GetXdgDirectory,
        ListDirectory,
        PathIsSymbolicLink
      ),
    XdgDirectory (XdgState),
  )
import Effectful.FileSystem.PathReader.Static qualified as PRStatic
import Effectful.FileSystem.PathWriter.Dynamic
  ( PathWriterDynamic
      ( CopyFileWithMetadata,
        CreateDirectory,
        CreateDirectoryIfMissing,
        RemoveDirectory,
        RemoveDirectoryRecursive,
        RemoveFile,
        RenameDirectory,
        RenameFile
      ),
  )
import Effectful.FileSystem.PathWriter.Static qualified as PWStatic
import Effectful.FileSystem.Utils (unsafeDecodeOsToFp, unsafeEncodeFpToOs)
import Effectful.Logger.Dynamic (LoggerDynamic (LoggerLog))
import Effectful.LoggerNS.Dynamic
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    LoggerNSDynamic (GetNamespace, LocalNamespace),
  )
import Effectful.LoggerNS.Dynamic qualified as Logger
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
    Window (Window),
  )
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Dynamic
  ( TimeDynamic (GetMonotonicTime, GetSystemZonedTime),
  )
import SafeRm qualified
import SafeRm.Data.Backend (Backend (BackendCbor, BackendFdo))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData (PathData (PathDataCbor, PathDataFdo))
import SafeRm.Data.PathData.Cbor qualified as Cbor
import SafeRm.Data.PathData.Fdo qualified as Fdo
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Env (HasBackend, HasTrashHome)
import SafeRm.Prelude
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Exit (die)

-- | The test runner.
type TestM a =
  Eff
    [ Reader TestEnv,
      IOE
    ]
    a

-- | The test environment
data TestEnv = MkTestEnv
  { backend :: Backend,
    -- The test dir relative to testRoot e.g. delete/deletesMany
    testDir :: OsPath,
    -- The trash dir relative to testDir e.g. .trash
    trashDir :: OsPath,
    -- Root.
    testRoot :: OsPath
  }

makeFieldLabelsNoPrefix ''TestEnv

-- | Runs a given test with the environment.
usingTestM :: TestEnv -> TestM a -> IO a
usingTestM env = runEff . runReader env

-- NOTE: Because this is used in FunctionalPrelude, we cannot use that import.

-- | Infinite stream of chars.
data CharStream = Char :> CharStream

infixr 5 :>

-- | Alternating stream ['n', 'y', 'n', ...]
altAnswers :: CharStream
altAnswers = 'n' :> 'y' :> altAnswers

-- | Safe-rm environment for functional tests.
data FuncEnv = MkFuncEnv
  { -- | Trash home.
    trashHome :: !(PathI TrashHome),
    backend :: !Backend,
    -- | Log namespace.
    logNamespace :: !Namespace,
    -- | Saves the terminal output.
    terminalRef :: !(IORef Text),
    -- | Saves the logs output.
    logsRef :: !(IORef Text),
    -- | Used to alternate responses to getChar.
    charStream :: !(IORef CharStream)
  }

instance Show FuncEnv where
  show (MkFuncEnv th backend ns _ _ _) =
    mconcat
      [ "MkFuncEnv {trashHome = ",
        show th,
        ", backend = ",
        show backend,
        ", logNamespace = ",
        show ns,
        ", terminalRef = <ref>, logsRef = <ref>, charStream = <ref> }"
      ]

makeFieldLabelsNoPrefix ''FuncEnv

deriving anyclass instance HasBackend FuncEnv

deriving anyclass instance HasTrashHome FuncEnv

runPathReader ::
  ( IOE :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (PathReaderDynamic : es) a ->
  Eff es a
runPathReader = reinterpret PRStatic.runPathReaderStaticIO $ \_ -> \case
  DoesFileExist p -> PRStatic.doesFileExist p
  DoesDirectoryExist p -> PRStatic.doesDirectoryExist p
  DoesPathExist p -> PRStatic.doesPathExist p
  ListDirectory p -> PRStatic.listDirectory p
  CanonicalizePath p -> PRStatic.canonicalizePath p
  PathIsSymbolicLink p -> PRStatic.pathIsSymbolicLink p
  GetFileSize _ -> pure 5
  -- Redirecting the xdg state to the trash dir so that we do not interact with
  -- the real state (~/.local/state/safe-rm) and instead use our testing
  -- trash dir
  GetXdgDirectory xdg p ->
    case xdg of
      XdgState -> do
        MkPathI th <- asks @FuncEnv (view #trashHome)
        pure th
      _ -> PRStatic.getXdgDirectory xdg p
  _ -> error "Functional.Prelude.FuncEnv.runPathReader: unimplemented"

runPathWriter ::
  ( IOE :> es
  ) =>
  Eff (PathWriterDynamic : es) a ->
  Eff es a
runPathWriter = reinterpret PWStatic.runPathWriterStaticIO $ \_ -> \case
  CreateDirectory p -> PWStatic.createDirectory p
  CreateDirectoryIfMissing b p -> PWStatic.createDirectoryIfMissing b p
  RenameDirectory d1 d2 -> PWStatic.renameDirectory d1 d2
  RenameFile f1 f2 -> PWStatic.renameFile f1 f2
  RemoveDirectory d -> PWStatic.removeDirectory d
  RemoveDirectoryRecursive d -> PWStatic.removeDirectoryRecursive d
  CopyFileWithMetadata f1 f2 -> PWStatic.copyFileWithMetadata f1 f2
  RemoveFile x
    -- This is for X.deletesSomeWildcards test
    | (T.isSuffixOf "fooBadbar" $ T.pack $ unsafeDecodeOsToFp x) ->
        throwString "Mock: cannot delete fooBadbar"
    | otherwise -> PWStatic.removeFile x
  _ -> error "Functional.Prelude.FuncEnv.runPathWriter: unimplemented"

runTerminal ::
  ( IORefStatic :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (TerminalDynamic : es) a ->
  Eff es a
runTerminal = interpret $ \_ -> \case
  PutStr s ->
    asks @FuncEnv (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  GetChar -> do
    charStream <- asks @FuncEnv (view #charStream)
    c :> cs <- readIORef charStream
    writeIORef charStream cs
    pure c
  GetTerminalSize ->
    pure
      $ Window
        { height = 50,
          width = 100
        }
  PutStrLn s ->
    asks @FuncEnv (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack (s <> "\n"))
  PutBinary _ -> error "PutBinary: unimplemented"
  GetLine -> error "GetLine: unimplemented"
  GetContents' -> error "GetContents': unimplemented"

runTime :: Eff (TimeDynamic : es) a -> Eff es a
runTime = interpret $ \_ -> \case
  GetSystemZonedTime -> pure $ ZonedTime localTime utc
  GetMonotonicTime -> pure 0

fixedTimestamp :: Timestamp
fixedTimestamp = MkTimestamp localTime

localTime :: LocalTime
localTime = LocalTime (toEnum 59_000) midday

runLogger ::
  ( Reader FuncEnv :> es,
    IORefStatic :> es,
    LoggerNSDynamic :> es,
    TimeDynamic :> es
  ) =>
  Eff (LoggerDynamic : es) a ->
  Eff es a
runLogger = interpret $ \_ -> \case
  LoggerLog loc _src lvl msg -> do
    formatted <- Logger.formatLog (mkFormatter loc) lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks @FuncEnv (view #logsRef)
    modifyIORef' logsRef (<> txt)
    where
      mkFormatter l =
        MkLogFormatter
          { newline = True,
            locStrategy = LocStable l,
            timezone = False
          }

runLoggerNS :: (Reader FuncEnv :> es) => Eff (LoggerNSDynamic : es) a -> Eff es a
runLoggerNS = interpret $ \env -> \case
  GetNamespace -> asks @FuncEnv (view #logNamespace)
  LocalNamespace f m -> localSeqUnlift env $ \runner ->
    local @FuncEnv (over' #logNamespace f) (runner m)

-- | Runs a safe-rm action.
runFuncIO ::
  Eff
    [ FileReaderDynamic,
      FileWriterDynamic,
      HandleWriterDynamic,
      LoggerDynamic,
      LoggerNSDynamic,
      PathReaderDynamic,
      PathWriterDynamic,
      PosixCompatStatic,
      TerminalDynamic,
      TimeDynamic,
      IORefStatic,
      Reader FuncEnv,
      Concurrent,
      IOE
    ]
    a ->
  FuncEnv ->
  IO a
runFuncIO m env =
  runEff
    . runConcurrent
    . runReader env
    . runIORefStaticIO
    . runTime
    . runTerminal
    . runPosixCompatStaticIO
    . runPathWriter
    . runPathReader
    . runLoggerNS
    . runLogger
    . runHandleWriterDynamicIO
    . runFileWriterDynamicIO
    . runFileReaderDynamicIO
    $ m

-- | Creates the environment for safe-rm.
mkFuncEnv ::
  (MonadIO m) =>
  TomlConfig ->
  IORef Text ->
  IORef Text ->
  m FuncEnv
mkFuncEnv toml logsRef terminalRef = do
  trashHome <- liftIO getTrashHome
  charStream <- liftIO $ newIORef altAnswers
  pure
    $ MkFuncEnv
      { trashHome = trashHome,
        backend = fromMaybe BackendCbor (toml ^. #backend),
        terminalRef,
        logsRef,
        logNamespace = "functional",
        charStream
      }
  where
    getTrashHome = case toml ^. #trashHome of
      Nothing -> die "Setup error, no trash home on config"
      Just th -> pure th

-- | Runs safe-rm.
runSafeRm :: [String] -> TestM ()
runSafeRm = void . captureSafeRm

-- | Runs safe-rm and captures terminal output.
captureSafeRm :: [String] -> TestM [Text]
captureSafeRm = fmap (view _1) . captureSafeRmLogs

-- | Runs safe-rm and captures (terminal output, logs).
captureSafeRmLogs ::
  -- Args.
  [String] ->
  TestM ([Text], [Text])
captureSafeRmLogs argList = liftIO $ do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <-
    runEff
      $ EffEnv.runEnvironment
      $ runFileReaderDynamicIO
      $ runOptparseStaticIO
      $ runPathReaderDynamicIO getConfig
  env :: FuncEnv <- mkFuncEnv toml logsRef terminalRef

  _ <-
    runFuncIO (Runner.runCmd @FuncEnv cmd) env
      `catchAny` \ex -> do
        putStrLn "TERMINAL"
        readIORef terminalRef >>= putStrLn . T.unpack
        putStrLn "\n\nLOGS"
        readIORef logsRef >>= putStrLn . T.unpack
        putStrLn ""
        throwM ex

  terminal <- T.lines <$> readIORef terminalRef
  logs <- T.lines <$> readIORef logsRef

  pure (terminal, logs)
  where
    argList' = "-c" : "none" : argList
    getConfig = EffEnv.withArgs argList' Runner.getConfiguration

-- | Runs SafeRm, catching the expected exception.
runSafeRmException :: forall e. (Exception e) => [String] -> TestM ()
runSafeRmException = void . captureSafeRmExceptionLogs @e

-- | Runs safe-rm and captures a thrown exception and logs.
captureSafeRmExceptionLogs ::
  forall e.
  (Exception e) =>
  -- Args.
  [String] ->
  TestM (Text, [Text])
captureSafeRmExceptionLogs argList = liftIO $ do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <-
    runEff
      $ EffEnv.runEnvironment
      $ runFileReaderDynamicIO
      $ runOptparseStaticIO
      $ runPathReaderDynamicIO getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  let runCatch = do
        result <- try @_ @e $ (runFuncIO (Runner.runCmd @FuncEnv cmd) env)

        case result of
          Right _ ->
            error
              "captureSafeRmExceptionLogs: Expected exception, received none"
          Left ex -> do
            logs <- T.lines <$> readIORef logsRef
            pure (T.pack (displayException ex), logs)

  runCatch
    `catchAny` \ex -> do
      -- Handle any uncaught exceptions
      putStrLn "TERMINAL"
      readIORef terminalRef >>= putStrLn . T.unpack
      putStrLn "\n\nLOGS"
      readIORef logsRef >>= putStrLn . T.unpack
      putStrLn ""
      throwM ex
  where
    argList' = "-c" : "none" : argList
    getConfig = EffEnv.withArgs argList' Runner.getConfiguration

-- | Runs SafeRm's Index and Metadata commands, using the TestEnv to determine
-- the test directory.
runIndexMetadataM :: TestM (HashSet PathData, Metadata)
runIndexMetadataM = do
  testDir <- getTestDir
  runIndexMetadataTestDirM testDir

-- | Runs SafeRm's Index and Metadata commands with the given test directory.
runIndexMetadataTestDirM :: OsPath -> TestM (HashSet PathData, Metadata)
runIndexMetadataTestDirM testDir = do
  env :: TestEnv <- ask
  terminalRef <- liftIO $ newIORef ""
  logsRef <- liftIO $ newIORef ""
  charStream <- liftIO $ newIORef altAnswers

  let trashDir = env ^. #trashDir
      backend = env ^. #backend
      funcEnv :: FuncEnv
      funcEnv =
        MkFuncEnv
          { trashHome = MkPathI (testDir </> trashDir),
            backend,
            logNamespace = "functional",
            terminalRef,
            logsRef,
            charStream
          }

  -- Need to canonicalize due to windows aliases
  tmpDir <- liftIO $ PRStatic.canonicalizePath =<< PRStatic.getTemporaryDirectory

  idx <- liftIO $ view #unIndex <$> runFuncIO (SafeRm.getIndex @FuncEnv) funcEnv
  mdata <- liftIO $ runFuncIO (SafeRm.getMetadata @FuncEnv) funcEnv

  pure (foldl' (addSet tmpDir) HSet.empty idx, mdata)
  where
    addSet :: OsPath -> HashSet PathData -> PathData -> HashSet PathData
    addSet tmp acc pd =
      let fixPath =
            MkPathI
              . unsafeEncodeFpToOs
              . T.unpack
              . T.replace (T.pack $ unsafeDecodeOsToFp tmp) ""
              . T.pack
              . unsafeDecodeOsToFp
              . view #unPathI
       in case pd of
            PathDataCbor d -> HSet.insert (PathDataCbor $ over' #originalPath fixPath d) acc
            PathDataFdo d -> HSet.insert (PathDataFdo $ over' #originalPath fixPath d) acc

-- | Transforms the list of filepaths into a Set PathData i.e. for each @p@,
--
-- @
-- { fileName = p,
--   originalPath = testDir </> p
--   created = fixedTimestamp
-- }
-- @
--
-- The type of PathData that is returned depends on the test env's current
-- backend.
mkPathDataSetM :: [String] -> TestM (HashSet PathData)
mkPathDataSetM paths = do
  testDir <- getTestDir
  mkPathDataSetTestDirM testDir paths

-- | Like 'mkPathDataSetM', except uses the given path as the test directory,
-- rather than having it determined by the TestEnv.
mkPathDataSetTestDirM :: OsPath -> [String] -> TestM (HashSet PathData)
mkPathDataSetTestDirM testDir paths = do
  env :: TestEnv <- ask
  let backend = env ^. #backend

  tmpDir <- liftIO $ PRStatic.canonicalizePath =<< PRStatic.getTemporaryDirectory
  let testDir' =
        unsafeEncodeFpToOs
          $ T.unpack
          $ T.replace (T.pack $ unsafeDecodeOsToFp tmpDir) "" (T.pack $ unsafeDecodeOsToFp testDir)

  pure
    $ HSet.fromList
    $ paths
    <&> \p ->
      let p' = unsafeEncodeFpToOs p
       in case backend of
            BackendCbor ->
              PathDataCbor
                $ Cbor.UnsafePathData
                  { fileName = MkPathI p',
                    originalPath = MkPathI (testDir' </> p'),
                    created = fixedTimestamp
                  }
            BackendFdo ->
              PathDataFdo
                $ Fdo.UnsafePathData
                  { fileName = MkPathI p',
                    originalPath = MkPathI (testDir' </> p'),
                    created = fixedTimestamp
                  }

-- | Like 'mkPathDataSetM', except takes two paths for when the fileName and
-- originalPath differ i.e. for each @(p, q)@
--
-- @
-- { fileName = p,
--   originalPath = testDir </> q
--   created = fixedTimestamp
-- }
-- @
mkPathDataSetM2 :: [(String, String)] -> TestM (HashSet PathData)
mkPathDataSetM2 paths = do
  env :: TestEnv <- ask
  let backend = env ^. #backend

  testDir <- getTestDir
  tmpDir <- liftIO $ PRStatic.canonicalizePath =<< PRStatic.getTemporaryDirectory
  let testDir' =
        unsafeEncodeFpToOs
          $ T.unpack
          $ T.replace
            (T.pack $ unsafeDecodeOsToFp tmpDir)
            ""
            (T.pack $ unsafeDecodeOsToFp testDir)

  pure
    $ HSet.fromList
    $ paths
    <&> \(fn, opath) ->
      let fn' = unsafeEncodeFpToOs fn
          opath' = unsafeEncodeFpToOs opath
       in case backend of
            BackendCbor ->
              PathDataCbor
                $ Cbor.UnsafePathData
                  { fileName = MkPathI fn',
                    originalPath = MkPathI (testDir' </> opath'),
                    created = fixedTimestamp
                  }
            BackendFdo ->
              PathDataFdo
                $ Fdo.UnsafePathData
                  { fileName = MkPathI fn',
                    originalPath = MkPathI (testDir' </> opath'),
                    created = fixedTimestamp
                  }

-- | Returns the full test dir for the given environment i.e.
--
-- <trashRoot>/<testDir>-<backend>
--
-- e.g.
--
-- /tmp/safe-rm/functional/delete/deleteOne-fdo
getTestDir :: TestM OsPath
getTestDir = do
  env :: TestEnv <- ask

  -- See NOTE: [OSX temp symlink]
  testRoot <- liftIO $ PRStatic.canonicalizePath (env ^. #testRoot)

  let testDir =
        testRoot
          </> (env ^. #testDir)
          <> [osp|-|]
          <> Backend.backendArgOsPath (env ^. #backend)
  liftIO $ PWStatic.createDirectoryIfMissing True testDir

  pure testDir
