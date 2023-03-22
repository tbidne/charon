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

    -- ** Runners
    runSafeRm,
    runSafeRmException,
    runIndexMetadata,

    -- *** Data capture
    captureSafeRm,
    captureSafeRmLogs,
    captureSafeRmExceptionLogs,

    -- * Misc
    mkPathData,
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
import Numeric.Literal.Integer as X (FromInteger (afromInteger))
import PathSize qualified
import SafeRm qualified
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData.Internal (PathData (..))
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Timestamp (Timestamp (..))
import SafeRm.Env (HasTrashHome)
import SafeRm.Prelude
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Environment qualified as SysEnv
import System.Exit (die)
import Test.Utils qualified as U

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
  show (MkFuncEnv th ns _ _ _) =
    mconcat
      [ "MkFuncEnv {trashHome = ",
        show th,
        ", logNamespace = ",
        show ns,
        ", terminalRef = <ref>, logsRef = <ref>, charStream = <ref> }"
      ]

makeFieldLabelsNoPrefix ''FuncEnv

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
  createDirectoryIfMissing b = liftIO . createDirectoryIfMissing b
  renameDirectory x = liftIO . renameDirectory x
  renameFile x = liftIO . renameFile x
  removeDirectory = liftIO . removeDirectory
  removeDirectoryRecursive = liftIO . removeDirectoryRecursive

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

mkFuncEnv :: (HasCallStack) => TomlConfig -> IORef Text -> IORef Text -> IO FuncEnv
mkFuncEnv toml logsRef terminalRef = do
  trashHome <- getTrashHome
  charStream <- newIORef altAnswers
  pure $
    MkFuncEnv
      { trashHome = trashHome,
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
runSafeRm :: [String] -> IO ()
runSafeRm = void . captureSafeRm

-- | Runs safe-rm and captures terminal output.
captureSafeRm :: [String] -> IO [Text]
captureSafeRm = fmap (view _1) . captureSafeRmLogs

-- | Runs safe-rm and captures (terminal output, logs).
captureSafeRmLogs ::
  -- Args.
  [String] ->
  IO ([Text], [Text])
captureSafeRmLogs argList = do
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

runSafeRmException :: forall e. (Exception e) => [String] -> IO ()
runSafeRmException = void . captureSafeRmExceptionLogs @e

-- | Runs safe-rm and captures a thrown exception and logs.
captureSafeRmExceptionLogs ::
  forall e.
  (Exception e) =>
  -- Args.
  [String] ->
  IO (Text, [Text])
captureSafeRmExceptionLogs argList = do
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

runIndexMetadata :: FilePath -> IO (HashSet PathData, Metadata)
runIndexMetadata testDir = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""
  charStream <- newIORef altAnswers

  let funcEnv =
        MkFuncEnv
          { trashHome = MkPathI (testDir </> ".trash"),
            logNamespace = "functional",
            terminalRef,
            logsRef,
            charStream
          }

  -- Need to canonicalize due to windows aliases
  tmpDir <- canonicalizePath =<< getTemporaryDirectory

  idx <- view #unIndex <$> runFuncIO SafeRm.getIndex funcEnv
  mdata <- runFuncIO SafeRm.getMetadata funcEnv

  pure (foldl' (addSet tmpDir) HSet.empty idx, mdata)
  where
    addSet tmp acc pd =
      let fixPath =
            MkPathI
              . T.unpack
              . T.replace (T.pack tmp) ""
              . T.pack
              . view #unPathI
          pd' =
            UnsafePathData
              { pathType = pd ^. #pathType,
                fileName = pd ^. #fileName,
                originalPath = fixPath (pd ^. #originalPath),
                size = pd ^. #size,
                created = pd ^. #created
              }
       in HSet.insert pd' acc

mkPathData ::
  PathType ->
  PathI TrashName ->
  PathI OriginalPath ->
  PathData
mkPathData pathType fileName originalPath =
  UnsafePathData
    { pathType,
      fileName,
      originalPath = U.massagePathI originalPath,
      size = afromInteger 5,
      created = fixedTimestamp
    }
