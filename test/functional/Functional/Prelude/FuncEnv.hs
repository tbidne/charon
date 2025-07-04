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

    -- * HUnit
    (@=?),

    -- * Running Charon
    TestM,
    TestEnv (..),

    -- ** Runners
    runCharon,
    runCharonEnv,
    runCharonE,

    -- *** Data capture
    captureCharon,
    captureCharonEnv,
    captureCharonLogs,
    captureCharonEnvLogs,
    captureCharonException,
    captureCharonLogsE,
    captureCharonTermE,

    -- * Misc
    getTestDir,
    assertFdoDirectorySizesM,
    assertFdoDirectorySizesTestDirM,
    assertFdoDirectorySizesArgsM,
    assertFdoDirectorySizesArgsNoOrderM,
  )
where

import Charon.Backend.Data (Backend (BackendCbor, BackendFdo))
import Charon.Backend.Data qualified as Backend
import Charon.Backend.Fdo.DirectorySizes (DirectorySizes (MkDirectorySizes))
import Charon.Backend.Fdo.DirectorySizes qualified as DirectorySizes
import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import Charon.Env (HasBackend, HasTrashHome)
import Charon.Prelude
import Charon.Runner qualified as Runner
import Charon.Runner.Toml (TomlConfigP2)
import Charon.Utils qualified as Utils
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effects.FileSystem.PathReader (XdgDirectory (XdgState))
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Effects.Logger.Namespace
  ( LocStrategy (LocPartial),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
  )
import Effects.Logger.Namespace qualified as Logger
import Effects.System.Terminal (Window (Window))
import Effects.System.Terminal qualified as Term
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
  )
import FileSystem.OsPath (unsafeDecode)
import GHC.Exts (IsList (toList))
import System.Environment qualified as SysEnv
import System.Exit (die)
import Test.Tasty.HUnit (assertBool)
import Test.Tasty.HUnit qualified as HUnit

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
    namespace :: Namespace,
    -- | Saves the terminal output.
    terminalRef :: IORef Text,
    -- | Saves the logs output.
    logsRef :: IORef Text,
    -- | Used to alternate responses to getChar.
    charStream :: IORef CharStream,
    -- | Terminal answer to getLine.
    strLine :: String
  }

instance Show FuncEnv where
  show (MkFuncEnv th backend ns _ _ _ strLine) =
    mconcat
      [ "MkFuncEnv {trashHome = ",
        show th,
        ", backend = ",
        show backend,
        ", logNamespace = ",
        show ns,
        ", terminalRef = <ref>, logsRef = <ref>, charStream = <ref>, strLine = ",
        show strLine,
        "}"
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
      MonadThread,
      MonadThrow,
      MonadReader env
    )
    via (ReaderT env IO)

#if !WINDOWS
deriving newtype instance MonadPosix (FuncIO env)
#endif

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

  -- Real size. This makes our lives easier as we do not have to mock it
  -- (hence can use difficult to mock posix functions directly), though it
  -- means we need separate files for windows/osx/linux.
  getFileSize = liftIO . PR.getFileSize
  getSymbolicLinkTarget = liftIO . PR.getSymbolicLinkTarget

  -- Redirecting the xdg state to the trash dir so that we do not interact with
  -- the real state (~/.local/state/charon) and instead use our testing
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
    | (T.isSuffixOf "fooBadbar" $ T.pack $ unsafeDecode x) =
        throwString "Mock: cannot delete fooBadbar"
    | otherwise = liftIO $ PW.removeFile x

instance
  ( Is k1 A_Getter,
    LabelOptic' "terminalRef" k1 env (IORef Text)
  ) =>
  MonadTerminal (FuncIO env)
  where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)

  getTerminalSize =
    pure
      $ Window
        { height = 50,
          width = 100
        }

  supportsPretty = pure False

instance
  ( Is k1 A_Getter,
    LabelOptic' "charStream" k1 env (IORef CharStream),
    Is k2 A_Getter,
    LabelOptic' "strLine" k2 env String,
    Is k3 A_Getter,
    LabelOptic' "terminalRef" k3 env (IORef Text)
  ) =>
  MonadHaskeline (FuncIO env)
  where
  getInputChar prompt = do
    asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack prompt)
    charStream <- asks (view #charStream)
    c :> cs <- readIORef charStream
    writeIORef charStream cs
    pure $ Just c

  getInputLine prompt = do
    asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack prompt)
    Just <$> asks (view #strLine)

instance MonadTime (FuncIO env) where
  getSystemZonedTime = pure $ ZonedTime localTime utc
  getMonotonicTime = pure 0

localTime :: LocalTime
localTime = LocalTime (toEnum 59_000) midday

localTimeMillis :: Natural
localTimeMillis = fromIntegral $ Utils.localTimeToMillis localTime

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
            threadLabel = False,
            timezone = False
          }

runFuncIO :: (FuncIO env) a -> env -> IO a
runFuncIO (MkFuncIO rdr) = runReaderT rdr

mkFuncEnv ::
  (HasCallStack, MonadIO m) =>
  TomlConfigP2 ->
  IORef Text ->
  IORef Text ->
  m FuncEnv
mkFuncEnv toml logsRef terminalRef = do
  trashHome <- liftIO getTrashHome'
  charStream <- liftIO $ newIORef altAnswers
  pure
    $ MkFuncEnv
      { trashHome = trashHome,
        backend = fromMaybe BackendCbor (toml ^. #backend),
        terminalRef,
        logsRef,
        namespace = "functional",
        charStream,
        strLine = "mkFuncEnv_answer"
      }
  where
    getTrashHome' = case toml ^. #trashHome of
      Nothing -> die "Setup error, no trash home on config"
      Just th -> pure th

-- | Runs charon.
runCharon :: (MonadIO m) => [String] -> m ()
runCharon = void . captureCharon

runCharonEnv :: (MonadIO m) => (FuncEnv -> FuncEnv) -> [String] -> m ()
runCharonEnv modEnv = void . captureCharonEnv modEnv

-- | Runs charon and captures terminal output.
captureCharon :: (MonadIO m) => [String] -> m [Text]
captureCharon = fmap (view _1) . captureCharonLogs

-- | Runs charon and captures terminal output.
captureCharonEnv :: (MonadIO m) => (FuncEnv -> FuncEnv) -> [String] -> m [Text]
captureCharonEnv modEnv = fmap (view _1) . captureCharonEnvLogs modEnv

-- | Runs charon and captures (terminal output, logs).
captureCharonLogs ::
  (MonadIO m) =>
  -- Args.
  [String] ->
  m ([Text], [Text])
captureCharonLogs = captureCharonEnvLogs id

-- | Runs charon and captures (terminal output, logs).
captureCharonEnvLogs ::
  (MonadIO m) =>
  -- | Env modifier
  (FuncEnv -> FuncEnv) ->
  -- Args.
  [String] ->
  m ([Text], [Text])
captureCharonEnvLogs modEnv argList = liftIO $ do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- modEnv <$> mkFuncEnv toml logsRef terminalRef

  catchesSync
    (runFuncIO (Runner.runCmd cmd) env)
    (handleEx terminalRef logsRef)
    [ -- Restore and Perm Delete can throw ExitSuccess. We do not want to
      -- error in these cases.
      Handler $ \(ex :: ExitCode) -> case ex of
        ExitSuccess -> pure ()
        ExitFailure _ -> handleEx terminalRef logsRef ex
    ]

  terminal <- T.lines <$> readIORef terminalRef
  logs <- T.lines <$> readIORef logsRef

  pure (terminal, logs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

    handleEx terminalRef logsRef ex = do
      putStrLn "TERMINAL"
      readIORef terminalRef >>= putStrLn . T.unpack
      putStrLn "\n\nLOGS"
      readIORef logsRef >>= putStrLn . T.unpack
      Utils.putLine
      throwM ex

-- | Runs Charon, catching the expected exception.
runCharonE :: forall e m. (Exception e, MonadIO m) => [String] -> m ()
runCharonE = void . captureCharonLogsE @e

-- | Runs charon and captures a thrown exception.
captureCharonException ::
  forall e m.
  (Exception e, MonadIO m) =>
  -- Args.
  [String] ->
  m Text
captureCharonException = fmap (view _1) . captureCharonLogsE @e

-- | Runs charon and captures a thrown exception and logs.
captureCharonLogsE ::
  forall e m.
  (Exception e, MonadIO m) =>
  -- Args.
  [String] ->
  m (Text, [Text])
captureCharonLogsE =
  fmap (\(ex, _, ls) -> (ex, ls))
    . captureCharonExceptionTerminalLogs @e

-- | Runs charon and captures a thrown exception and terminal.
captureCharonTermE ::
  forall e m.
  (Exception e, MonadIO m) =>
  -- Args.
  [String] ->
  m (Text, [Text])
captureCharonTermE =
  fmap (\(ex, term, _) -> (ex, term))
    . captureCharonExceptionTerminalLogs @e

-- | Runs charon and captures a thrown exception and logs.
captureCharonExceptionTerminalLogs ::
  forall e m.
  (Exception e, MonadIO m) =>
  -- Args.
  [String] ->
  m (Text, [Text], [Text])
captureCharonExceptionTerminalLogs argList = liftIO $ do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  try @_ @e getConfig >>= \case
    Left ex -> pure (T.pack (displayException ex), [], [])
    Right (toml, cmd) -> do
      env <- mkFuncEnv toml logsRef terminalRef

      let runCatch = do
            result <- try @_ @e $ runFuncIO (Runner.runCmd cmd) env

            case result of
              Right _ ->
                error
                  "captureCharonLogsE: Expected exception, received none"
              Left ex -> do
                term <- T.lines <$> readIORef terminalRef
                logs <- T.lines <$> readIORef logsRef
                pure (T.pack (displayException ex), term, logs)

      runCatch
        `catchSync` \ex -> do
          -- Handle any uncaught exceptions
          putStrLn "TERMINAL"
          readIORef terminalRef >>= putStrLn . T.unpack
          putStrLn "\n\nLOGS"
          readIORef logsRef >>= putStrLn . T.unpack
          Utils.putLine
          throwM ex
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Returns the full test dir for the given environment i.e.
--
-- <trashRoot>/<testDir>-<backend>
--
-- e.g.
--
-- /tmp/charon/functional/delete/deleteOne-fdo
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

assertFdoDirectorySizesM :: [ByteString] -> TestM ()
assertFdoDirectorySizesM expectedFileNames = do
  testDir <- getTestDir
  assertFdoDirectorySizesTestDirM testDir expectedFileNames

assertFdoDirectorySizesTestDirM :: OsPath -> [ByteString] -> TestM ()
assertFdoDirectorySizesTestDirM testDir expectedFileNames = do
  backend <- asks (view #backend)
  assertFdoDirectorySizesArgsM backend testDir expectedFileNames

assertFdoDirectorySizesArgsM :: Backend -> OsPath -> [ByteString] -> TestM ()
assertFdoDirectorySizesArgsM backend testDir expectedFileNames = do
  case backend of
    BackendFdo -> do
      trashDir <- asks (view #trashDir)

      let directorySizesPath = testDir </> trashDir

      MkDirectorySizes directorySizes <- DirectorySizes.readDirectorySizesTrashHome (MkPathI directorySizesPath)

      let directorySizes' = toList directorySizes
          errMsg =
            mconcat
              [ "Expected:\n",
                show expectedFileNames,
                "\n\n",
                "Results:\n",
                show directorySizes'
              ]

      liftIO $ assertBool errMsg (length expectedFileNames == length directorySizes')

      for_ (L.zip expectedFileNames directorySizes') $ \(expectedFileName, result) -> do
        localTimeMillis @=? result ^. #time
        expectedFileName @=? result ^. #fileName
    _ -> pure ()

assertFdoDirectorySizesArgsNoOrderM :: Backend -> OsPath -> [ByteString] -> TestM ()
assertFdoDirectorySizesArgsNoOrderM backend testDir expectedFileNames = do
  case backend of
    BackendFdo -> do
      trashDir <- asks (view #trashDir)

      let directorySizesPath = testDir </> trashDir

      MkDirectorySizes results <- DirectorySizes.readDirectorySizesTrashHome (MkPathI directorySizesPath)

      let resultsSorted =
            L.sortOn (view #fileName) $ toList results
          errMsg =
            mconcat
              [ "Expected:\n",
                show expectedFileNames,
                "\n\n",
                "Results:\n",
                show resultsSorted
              ]

          expectedFileNamesSorted = L.sort expectedFileNames

      liftIO $ assertBool errMsg (length expectedFileNames == length resultsSorted)

      for_ (L.zip expectedFileNamesSorted resultsSorted) $ \(expectedFileName, result) -> do
        localTimeMillis @=? result ^. #time
        expectedFileName @=? result ^. #fileName
    _ -> pure ()

-- | Lifted (@=?).
(@=?) :: (Eq a, HasCallStack, MonadIO m, Show a) => a -> a -> m ()
x @=? y = liftIO $ x HUnit.@=? y

infix 1 @=?
