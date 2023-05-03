{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude.FuncEnv
  ( -- * Types
    FuncIO (..),
    runFuncIO,
    FuncEnv (..),
    mkFuncEnv,

    -- * Running SafeRm
    TestM,
    TestEnv (..),

    -- ** Runners
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
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (..))
import Data.Time.LocalTime (midday, utc)
import Effects.FileSystem.PathReader (MonadPathReader (..), XdgDirectory (XdgState))
import Effects.FileSystem.PathWriter (MonadPathWriter (..))
import Effects.LoggerNS
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNS qualified as Logger
import Effects.System.Terminal (MonadTerminal (..), Window (..))
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
  )
import PathSize qualified
import SafeRm qualified
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData (PathData (..))
import SafeRm.Data.PathData.Cbor qualified as Cbor
import SafeRm.Data.PathData.Fdo qualified as Fdo
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Timestamp (Timestamp (..))
import SafeRm.Env (HasBackend, HasTrashHome)
import SafeRm.Prelude
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Environment qualified as SysEnv
import System.Exit (die)

type TestM a = ReaderT TestEnv IO a

data TestEnv = MkTestEnv
  { backend :: Backend,
    -- The test dir relative to testRoot e.g. delete/deletesMany
    testDir :: FilePath,
    -- The trash dir relative to testDir e.g. .trash
    trashDir :: FilePath,
    -- Root.
    testRoot :: FilePath
  }

makeFieldLabelsNoPrefix ''TestEnv

-- NOTE: Because this is used in FunctionalPrelude, we cannot use that import.

-- | Infinite stream of chars.
data CharStream = Char :> CharStream

infixr 5 :>

-- | Alternating stream ['n', 'y', 'n', ...]
altAnswers :: CharStream
altAnswers = 'n' :> 'y' :> altAnswers

-- | Environment for running functional tests.
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

-- | Type for running functional tests.
newtype FuncIO env a = MkFuncIO (ReaderT env IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadThrow,
      MonadReader env
    )
    via (ReaderT env IO)

-- Overriding this for getXdgDirectory
instance
  ( Is k A_Getter,
    LabelOptic' "trashHome" k env (PathI TrashHome)
  ) =>
  MonadPathReader (FuncIO env)
  where
  doesFileExist = liftIO . doesFileExist
  doesDirectoryExist = liftIO . doesDirectoryExist
  doesPathExist = liftIO . doesPathExist
  listDirectory = liftIO . listDirectory
  canonicalizePath = liftIO . canonicalizePath
  getFileSize = liftIO . getFileSize

  -- Redirecting the xdg state to the trash dir so that we do not interact with
  -- the real state (~/.local/state/safe-rm) and instead use our testing
  -- trash dir
  getXdgDirectory XdgState _ = do
    MkPathI th <- asks (view #trashHome)
    pure th
  getXdgDirectory xdg p = liftIO $ getXdgDirectory xdg p

instance MonadPathWriter (FuncIO env) where
  createDirectory = liftIO . createDirectory
  createDirectoryIfMissing b = liftIO . createDirectoryIfMissing b
  renameDirectory x = liftIO . renameDirectory x
  renameFile x = liftIO . renameFile x
  removeDirectory = liftIO . removeDirectory
  removeDirectoryRecursive = liftIO . removeDirectoryRecursive
  copyFileWithMetadata src = liftIO . copyFileWithMetadata src

  removeFile x
    -- This is for X.deletesSomeWildcards test
    | (T.isSuffixOf "fooBadbar" $ T.pack x) = throwString "Mock: cannot delete fooBadbar"
    | otherwise = liftIO $ removeFile x

instance MonadPathSize (FuncIO env) where
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

instance
  ( Is k A_Getter,
    LabelOptic' "terminalRef" k env (IORef Text),
    Is l A_Getter,
    LabelOptic' "charStream" l env (IORef CharStream)
  ) =>
  MonadTerminal (FuncIO env)
  where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  getChar = do
    charStream <- asks (view #charStream)
    c :> cs <- readIORef charStream
    writeIORef charStream cs
    pure c
  getTerminalSize =
    pure $
      Window
        { height = 50,
          width = 100
        }

instance MonadTime (FuncIO env) where
  getSystemZonedTime = pure $ ZonedTime localTime utc
  getMonotonicTime = pure 0

fixedTimestamp :: Timestamp
fixedTimestamp = MkTimestamp localTime

localTime :: LocalTime
localTime = LocalTime (toEnum 59_000) midday

instance MonadLogger (FuncIO FuncEnv) where
  monadLoggerLog loc _src lvl msg = do
    formatted <- Logger.formatLog (mkFormatter loc) lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (view #logsRef)
    modifyIORef' logsRef (<> txt)
    where
      mkFormatter l =
        MkLogFormatter
          { newline = True,
            locStrategy = LocStable l,
            timezone = False
          }

instance MonadLoggerNS (FuncIO FuncEnv) where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

runFuncIO :: (FuncIO env) a -> env -> IO a
runFuncIO (MkFuncIO rdr) = runReaderT rdr

mkFuncEnv :: (HasCallStack, MonadIO m) => TomlConfig -> IORef Text -> IORef Text -> m FuncEnv
mkFuncEnv toml logsRef terminalRef = do
  trashHome <- liftIO getTrashHome
  charStream <- liftIO $ newIORef altAnswers
  pure $
    MkFuncEnv
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
runSafeRm :: (MonadIO m) => [String] -> m ()
runSafeRm = void . captureSafeRm

-- | Runs safe-rm and captures terminal output.
captureSafeRm :: (MonadIO m) => [String] -> m [Text]
captureSafeRm = fmap (view _1) . captureSafeRmLogs

-- | Runs safe-rm and captures (terminal output, logs).
captureSafeRmLogs ::
  (MonadIO m) =>
  -- Args.
  [String] ->
  m ([Text], [Text])
captureSafeRmLogs argList = liftIO $ do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  runFuncIO (Runner.runCmd cmd) env
    `catchAny` \ex -> do
      putStrLn "TERMINAL"
      readIORef terminalRef >>= putStrLn . T.unpack
      putStrLn "\n\nLOGS"
      readIORef logsRef >>= putStrLn . T.unpack
      putStrLn ""
      throwCS ex

  terminal <- T.lines <$> readIORef terminalRef
  logs <- T.lines <$> readIORef logsRef

  pure (terminal, logs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs SafeRm, catching the expected exception.
runSafeRmException :: forall e m. (Exception e, MonadIO m) => [String] -> m ()
runSafeRmException = void . captureSafeRmExceptionLogs @e

-- | Runs safe-rm and captures a thrown exception and logs.
captureSafeRmExceptionLogs ::
  forall e m.
  (Exception e, MonadIO m) =>
  -- Args.
  [String] ->
  m (Text, [Text])
captureSafeRmExceptionLogs argList = liftIO $ do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  let runCatch = do
        result <- tryCS @_ @e $ runFuncIO (Runner.runCmd cmd) env

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
      throwCS ex
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs SafeRm's Index and Metadata commands, using the TestEnv to determine
-- the test directory.
runIndexMetadataM :: TestM (HashSet PathData, Metadata)
runIndexMetadataM = do
  testDir <- getTestDir
  runIndexMetadataTestDirM testDir

-- | Runs SafeRm's Index and Metadata commands with the given test directory.
runIndexMetadataTestDirM :: FilePath -> TestM (HashSet PathData, Metadata)
runIndexMetadataTestDirM testDir = do
  env <- ask
  terminalRef <- newIORef ""
  logsRef <- newIORef ""
  charStream <- newIORef altAnswers

  let trashDir = env ^. #trashDir
      backend = env ^. #backend
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
  tmpDir <- canonicalizePath =<< getTemporaryDirectory

  idx <- liftIO $ view #unIndex <$> runFuncIO SafeRm.getIndex funcEnv
  mdata <- liftIO $ runFuncIO SafeRm.getMetadata funcEnv

  pure (foldl' (addSet tmpDir) HSet.empty idx, mdata)
  where
    addSet :: String -> HashSet PathData -> PathData -> HashSet PathData
    addSet tmp acc pd =
      let fixPath =
            MkPathI
              . T.unpack
              . T.replace (T.pack tmp) ""
              . T.pack
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
mkPathDataSetM :: [FilePath] -> TestM (HashSet PathData)
mkPathDataSetM paths = do
  testDir <- getTestDir
  mkPathDataSetTestDirM testDir paths

-- | Like 'mkPathDataSetM', except uses the given path as the test directory,
-- rather than having it determined by the TestEnv.
mkPathDataSetTestDirM :: FilePath -> [FilePath] -> TestM (HashSet PathData)
mkPathDataSetTestDirM testDir paths = do
  env <- ask
  let backend = env ^. #backend

  tmpDir <- canonicalizePath =<< getTemporaryDirectory
  let testDir' = T.unpack $ T.replace (T.pack tmpDir) "" (T.pack testDir)

  pure $
    HSet.fromList $
      paths
        <&> \p -> case backend of
          BackendCbor ->
            PathDataCbor $
              Cbor.UnsafePathData
                { fileName = MkPathI p,
                  originalPath = MkPathI (testDir' </> p),
                  created = fixedTimestamp
                }
          BackendFdo ->
            PathDataFdo $
              Fdo.UnsafePathData
                { fileName = MkPathI p,
                  originalPath = MkPathI (testDir' </> p),
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
mkPathDataSetM2 :: [(FilePath, FilePath)] -> TestM (HashSet PathData)
mkPathDataSetM2 paths = do
  env <- ask
  let backend = env ^. #backend

  testDir <- getTestDir
  tmpDir <- canonicalizePath =<< getTemporaryDirectory
  let testDir' = T.unpack $ T.replace (T.pack tmpDir) "" (T.pack testDir)

  pure $
    HSet.fromList $
      paths
        <&> \(fn, opath) -> case backend of
          BackendCbor ->
            PathDataCbor $
              Cbor.UnsafePathData
                { fileName = MkPathI fn,
                  originalPath = MkPathI (testDir' </> opath),
                  created = fixedTimestamp
                }
          BackendFdo ->
            PathDataFdo $
              Fdo.UnsafePathData
                { fileName = MkPathI fn,
                  originalPath = MkPathI (testDir' </> opath),
                  created = fixedTimestamp
                }

-- | Returns the full test dir for the given environment i.e.
--
-- <trashRoot>/<testDir>-<backend>
--
-- e.g.
--
-- /tmp/safe-rm/functional/delete/deleteOne-fdo
getTestDir :: TestM String
getTestDir = do
  env <- ask

  -- See Note [OSX temp symlink]
  testRoot <- canonicalizePath (env ^. #testRoot)

  let testDir =
        testRoot
          </> env ^. #testDir
            <> "-"
            <> Backend.backendArg (env ^. #backend)
  createDirectoryIfMissing True testDir

  pure testDir
