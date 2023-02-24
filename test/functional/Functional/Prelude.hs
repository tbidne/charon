{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Prelude for functional test suite.
--
-- @since 0.1
module Functional.Prelude
  ( module X,

    -- * Types
    FuncIO (..),
    runFuncIO,
    FuncEnv (..),
    CharStream,
    altAnswers,

    -- * Running SafeRm

    -- ** Runners
    runSafeRm,
    captureSafeRm,
    captureSafeRmLogs,
    captureSafeRmExceptionLogs,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
    assertDirectoriesExist,
    assertDirectoriesDoNotExist,

    -- * Misc
    mkAllTrashPaths,
    mkTrashPaths,
    mkTrashInfoPaths,
    fixRandomPaths,
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.List ((++))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Text.Internal (Text (..))
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Search qualified as TIS
import Data.Time (LocalTime (LocalTime), ZonedTime (..))
import Data.Time.LocalTime (midday, utc)
import Effects.FileSystem.PathReader (MonadPathReader (..), XdgDirectory (XdgState))
import Effects.LoggerNamespace
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNamespace qualified as Logger
import Effects.System.Terminal (MonadTerminal (..), Window (..))
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
  )
import GHC.Exts (IsList (Item, fromList, toList))
import PathSize qualified
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome)
import SafeRm.FileUtils as X
import SafeRm.Prelude as X
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Environment qualified as SysEnv
import System.Exit (die)
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.Golden as X (goldenVsString, goldenVsStringDiff)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )

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
      MonadPathWriter,
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
  getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getMonotonicTime = pure 0

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

instance MonadLoggerNamespace (FuncIO FuncEnv) where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

runFuncIO :: (FuncIO env) a -> env -> IO a
runFuncIO (MkFuncIO rdr) = runReaderT rdr

-- | Runs safe-rm.
runSafeRm :: [String] -> IO ()
runSafeRm = void . captureSafeRm ""

-- | Runs safe-rm and captures terminal output.
captureSafeRm :: Builder -> [String] -> IO CapturedOutput
captureSafeRm title = fmap (view _1) . captureSafeRmLogs title

-- | Runs safe-rm and captures (terminal output, logs).
captureSafeRmLogs ::
  -- | Title to add to captured output.
  Builder ->
  -- Args.
  [String] ->
  IO (CapturedOutput, CapturedOutput)
captureSafeRmLogs title argList = do
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

  tmpDir <- getTemporaryDirectory

  terminal <- unsafeReplaceDir tmpDir <$> readIORef terminalRef
  logs <- unsafeReplaceDir tmpDir <$> readIORef logsRef
  let terminalBs = Builder.byteString $ encodeUtf8 terminal
      logs' = fixRandomPaths logs
      logsBs = Builder.byteString $ encodeUtf8 logs'

  pure (MonadTerminal title terminalBs, Logs title logsBs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs safe-rm and captures a thrown exception and logs.
captureSafeRmExceptionLogs ::
  forall e.
  (Exception e) =>
  -- | Title to add to captured output.
  Builder ->
  -- Args.
  [String] ->
  IO (CapturedOutput, CapturedOutput)
captureSafeRmExceptionLogs title argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  tmpDir <- getTemporaryDirectory

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  let runCatch = do
        result <- tryCS @_ @e $ runFuncIO (Runner.runCmd cmd) env

        case result of
          Right _ ->
            error
              "captureSafeRmExceptionLogs: Expected exception, received none"
          Left ex -> do
            logs <- unsafeReplaceDir tmpDir <$> readIORef logsRef
            let exceptionBs = exToBuilder (Just tmpDir) ex
                logsBs = txtToBuilder logs
            pure (Exception title exceptionBs, Logs title logsBs)

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

-- | Asserts that files exist.
assertFilesExist :: [FilePath] -> IO ()
assertFilesExist paths =
  for_ paths $ \p -> do
    exists <- doesFileExist p
    assertBool ("Expected file to exist: " <> p) exists

-- | Asserts that files do not exist.
assertFilesDoNotExist :: [FilePath] -> IO ()
assertFilesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- doesFileExist p
    assertBool ("Expected file not to exist: " <> p) (not exists)

-- | Asserts that directories exist.
assertDirectoriesExist :: [FilePath] -> IO ()
assertDirectoriesExist paths =
  for_ paths $ \p -> do
    exists <- doesDirectoryExist p
    assertBool ("Expected directory to exist: " <> p) exists

-- | Asserts that directories do not exist.
assertDirectoriesDoNotExist :: [FilePath] -> IO ()
assertDirectoriesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- doesDirectoryExist p
    assertBool ("Expected directory not to exist: " <> p) (not exists)

mkFuncEnv :: TomlConfig -> IORef Text -> IORef Text -> IO FuncEnv
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

mkAllTrashPaths ::
  ( Functor f,
    IsList (f FilePath),
    Item (f FilePath) ~ FilePath
  ) =>
  FilePath ->
  f FilePath ->
  f FilePath
mkAllTrashPaths trashHome paths =
  fromList (toList trashPaths ++ toList trashInfoPaths)
  where
    trashPaths = mkTrashPaths trashHome paths
    trashInfoPaths = mkTrashInfoPaths trashHome paths

mkTrashInfoPaths ::
  ( Functor f
  ) =>
  FilePath ->
  f FilePath ->
  f FilePath
mkTrashInfoPaths trashHome = fmap mkTrashInfoPath
  where
    mkTrashInfoPath p = trashHome </> "info" </> p <> ".json"

mkTrashPaths ::
  ( Functor f
  ) =>
  FilePath ->
  f FilePath ->
  f FilePath
mkTrashPaths trashHome = fmap mkTrashPath
  where
    mkTrashPath p = trashHome </> "paths" </> p

-- | HACK: There is a log line that starts out 'Paths: ...' that is
-- non-deterministic because the listed paths can be in random order.
-- This means the log strings will not match, hence the tests can fail,
-- which actually happened on CI.
--
-- As a workaround, we remove this path names from the logs until we come
-- up with a more robust solution.
fixRandomPaths :: Text -> Text
fixRandomPaths = T.unlines . foldr (flip fixRandomPaths') [] . T.lines
  where
    fixRandomPaths' :: [Text] -> Text -> [Text]
    fixRandomPaths' acc (stripInfix "Path:" -> Just (p, _)) = p <> "Path:" <> " ..." : acc
    fixRandomPaths' acc (stripInfix "Paths:" -> Just (p, _)) = p <> "Paths:" <> " [...]" : acc
    fixRandomPaths' acc (stripInfix "Info:" -> Just (p, _)) = p <> "Info:" <> " [...]" : acc
    fixRandomPaths' acc t = t : acc

stripInfix :: Text -> Text -> Maybe (Text, Text)
stripInfix p@(Text _arr _off plen) t@(Text arr off len) =
  case TIS.indices p t of
    [] -> Nothing
    (x : _) -> Just (TI.text arr off x, TI.text arr (x + plen) (len - plen - x))
