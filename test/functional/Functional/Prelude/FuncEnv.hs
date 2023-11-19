{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
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
    captureSafeRmException,
    captureSafeRmExceptionLogs,

    -- * Misc
    mkPathDataSetM,
    mkPathDataSetM2,
    mkPathDataSetTestDirM,
    getTestDir,

    -- * Re-exports
    PathType (..),
  )
where

import Data.HashSet qualified as HSet
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effects.FileSystem.PathReader (XdgDirectory (XdgState))
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Effects.FileSystem.Utils (unsafeDecodeOsToFp, unsafeEncodeFpToOs)
import Effects.LoggerNS
  ( LocStrategy (LocPartial),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNS qualified as Logger
import Effects.System.Terminal (Window (Window))
import Effects.System.Terminal qualified as Term
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm qualified
import SafeRm.Backend.Data (Backend (BackendCbor))
import SafeRm.Backend.Data qualified as Backend
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData
  ( PathData
      ( UnsafePathData,
        created,
        fileName,
        originalPath,
        pathType,
        size
      ),
  )
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.PathType as X
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymlink
      ),
  )
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
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
    testDir :: OsPath,
    -- The trash dir relative to testDir e.g. .trash
    trashDir :: OsPath,
    -- Root.
    testRoot :: OsPath
  }

makeFieldLabelsNoPrefix ''TestEnv

-- NOTE: Because this is used in FunctionalPrelude, we cannot use that import.

-- | Infinite stream of chars. Lazy annotation needed due to -XStrictData.
data CharStream = Char :> ~CharStream

infixr 5 :>

-- | Alternating stream ['n', 'y', 'n', ...]
altAnswers :: CharStream
altAnswers = 'n' :> 'y' :> altAnswers

-- | Environment for running functional tests.
data FuncEnv = MkFuncEnv
  { -- | Trash home.
    trashHome :: PathI TrashHome,
    backend :: Backend,
    -- | Log namespace.
    logNamespace :: Namespace,
    -- | Saves the terminal output.
    terminalRef :: IORef Text,
    -- | Saves the logs output.
    logsRef :: IORef Text,
    -- | Used to alternate responses to getChar.
    charStream :: IORef CharStream
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
      MonadAsync,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadPosixCompat,
      MonadThrow,
      MonadReader env
    )
    via (ReaderT env IO)

-- Overriding this for getXdgDirectory

{- ORMOLU_DISABLE -}

instance
  ( Is k A_Getter,
    LabelOptic' "trashHome" k env (PathI TrashHome)
  ) =>
  MonadPathReader (FuncIO env)
  where
  doesFileExist = liftIO . PR.doesFileExist
  doesDirectoryExist = liftIO . PR.doesDirectoryExist
  doesPathExist = liftIO . PR.doesPathExist
  listDirectory = liftIO . PR.listDirectory
  canonicalizePath = liftIO . PR.canonicalizePath
  makeAbsolute = liftIO . PR.makeAbsolute
  pathIsSymbolicLink = liftIO . PR.pathIsSymbolicLink
  getTemporaryDirectory = liftIO PR.getTemporaryDirectory

#if WINDOWS
  -- Default to zero because windows symlinks are differently sized.
  -- This causes all sorts of annoyances, so it's easier just to set
  -- everything to zero for now.
  --
  -- See NOTE: [Windows getFileSize]
  getFileSize _ = pure 0
#else
  getFileSize _ = pure 5
#endif
  getSymbolicLinkTarget = liftIO . PR.getSymbolicLinkTarget

  -- Redirecting the xdg state to the trash dir so that we do not interact with
  -- the real state (~/.local/state/safe-rm) and instead use our testing
  -- trash dir
  getXdgDirectory XdgState _ = do
    MkPathI th <- asks (view #trashHome)
    pure th
  getXdgDirectory xdg p = liftIO $ PR.getXdgDirectory xdg p

{- ORMOLU_ENABLE -}

instance MonadPathWriter (FuncIO env) where
  createDirectory = liftIO . PW.createDirectory
  createDirectoryIfMissing b = liftIO . PW.createDirectoryIfMissing b
  renameDirectory x = liftIO . PW.renameDirectory x
  renameFile x = liftIO . PW.renameFile x
  renamePath x = liftIO . PW.renamePath x
  removeDirectory = liftIO . PW.removeDirectory
  removeDirectoryRecursive = liftIO . PW.removeDirectoryRecursive
  copyFileWithMetadata src = liftIO . PW.copyFileWithMetadata src
  createDirectoryLink src = liftIO . PW.createDirectoryLink src
  createFileLink src = liftIO . PW.createFileLink src
  removeDirectoryLink = liftIO . PW.removeDirectoryLink

  removeFile x
    -- This is for X.deletesSomeWildcards test
    | (T.isSuffixOf "fooBadbar" $ T.pack $ unsafeDecodeOsToFp x) =
        throwString "Mock: cannot delete fooBadbar"
    | otherwise = liftIO $ PW.removeFile x

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
    pure
      $ Window
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
            locStrategy = LocPartial l,
            timezone = False
          }

instance MonadLoggerNS (FuncIO FuncEnv) where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

runFuncIO :: (FuncIO env) a -> env -> IO a
runFuncIO (MkFuncIO rdr) = runReaderT rdr

mkFuncEnv :: (HasCallStack, MonadIO m) => TomlConfig -> IORef Text -> IORef Text -> m FuncEnv
mkFuncEnv toml logsRef terminalRef = do
  trashHome <- liftIO getTrashHome'
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
    getTrashHome' = case toml ^. #trashHome of
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
captureSafeRmException ::
  forall e m.
  (Exception e, MonadIO m) =>
  -- Args.
  [String] ->
  m Text
captureSafeRmException = fmap (view _1) . captureSafeRmExceptionLogs @e

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
runIndexMetadataTestDirM :: OsPath -> TestM (HashSet PathData, Metadata)
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
  tmpDir <- PR.canonicalizePath =<< PR.getTemporaryDirectory

  idx <- liftIO $ fmap (view _1) . view #unIndex <$> runFuncIO SafeRm.getIndex funcEnv
  mdata <- liftIO $ runFuncIO SafeRm.getMetadata funcEnv

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
       in HSet.insert (over' #originalPath fixPath pd) acc

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
mkPathDataSetM :: [(String, PathType, Integer)] -> TestM (HashSet PathData)
mkPathDataSetM paths = do
  testDir <- getTestDir
  mkPathDataSetTestDirM testDir paths

{- ORMOLU_DISABLE -}

-- | Like 'mkPathDataSetM', except uses the given path as the test directory,
-- rather than having it determined by the TestEnv.
mkPathDataSetTestDirM ::
  OsPath ->
  [(String, PathType, Integer)] ->
  TestM (HashSet PathData)
mkPathDataSetTestDirM testDir pathData = do
  tmpDir <- PR.canonicalizePath =<< PR.getTemporaryDirectory
  let testDir' =
        unsafeEncodeFpToOs
          $ T.unpack
          $ T.replace (T.pack $ unsafeDecodeOsToFp tmpDir) "" (T.pack $ unsafeDecodeOsToFp testDir)

  pure
    $ HSet.fromList
    $ pathData
    <&> \(p, pathType, _sizeNat) ->
      let p' = unsafeEncodeFpToOs p
      -- NOTE: [Windows getFileSize]
      --
      -- Unlike files, the symlinks sizes are not mocked. While our
      -- expected value of 5 seems to work for posix, on windows they
      -- apparently have size 0. We could try to mock these instead, however
      -- that is difficult as the relevant MonadPosixCompat function
      -- (getFileStatus) involves foreign pointers.
      --
      -- Thus we do the easy thing here and zero everything out.
#if WINDOWS
          size = afromInteger 0
#else
          size = afromInteger _sizeNat
#endif
       in UnsafePathData
            { fileName = MkPathI p',
              originalPath = MkPathI (testDir' </> p'),
              created = fixedTimestamp,
              pathType = pathType,
              size
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
mkPathDataSetM2 :: [(String, String, PathType, Integer)] -> TestM (HashSet PathData)
mkPathDataSetM2 pathData = do
  testDir <- getTestDir
  tmpDir <- PR.canonicalizePath =<< PR.getTemporaryDirectory
  let testDir' =
        unsafeEncodeFpToOs
          $ T.unpack
          $ T.replace
            (T.pack $ unsafeDecodeOsToFp tmpDir)
            ""
            (T.pack $ unsafeDecodeOsToFp testDir)

  pure
    $ HSet.fromList
    $ pathData
    <&> \(fn, opath, pathType, _sizeNat) ->
      -- See NOTE: [Windows getFileSize]
      let fn' = unsafeEncodeFpToOs fn
          opath' = unsafeEncodeFpToOs opath
#if WINDOWS
          size = afromInteger 0
#else
          size = afromInteger _sizeNat
#endif
       in UnsafePathData
            { fileName = MkPathI fn',
              originalPath = MkPathI (testDir' </> opath'),
              created = fixedTimestamp,
              pathType,
              size
            }

{- ORMOLU_ENABLE -}

-- | Returns the full test dir for the given environment i.e.
--
-- <trashRoot>/<testDir>-<backend>
--
-- e.g.
--
-- /tmp/safe-rm/functional/delete/deleteOne-fdo
getTestDir :: TestM OsPath
getTestDir = do
  env <- ask

  -- See Note [OSX temp symlink]
  testRoot <- PR.canonicalizePath (env ^. #testRoot)

  let testDir =
        testRoot
          </> Backend.backendArgOsPath (env ^. #backend)
          </> (env ^. #testDir)
  PW.createDirectoryIfMissing True testDir

  pure testDir
