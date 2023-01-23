{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
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

    -- ** Capturing output
    CapturedOutput (..),
    capturedToBs,
    diff,

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
    fixCallStack,
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime (..))
import Data.Time.LocalTime (midday, utc)
import Effects.LoggerNamespace
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNamespace qualified as Logger
import Effects.System.Terminal (MonadTerminal (..))
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
  )
import GHC.Stack.Types (CallStack (PushCallStack), SrcLoc (..))
import PathSize qualified
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
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
      MonadExit,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadPathReader,
      MonadPathWriter,
      MonadThrow,
      MonadReader env
    )
    via (ReaderT env IO)

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
  getTerminalSize = liftIO getTerminalSize

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

-- | Represents captured input of some kind. Different constructors are
-- to make golden tests easier to understand (i.e. included labels)
data CapturedOutput
  = MonadTerminal Builder Builder
  | Logs Builder Builder
  | Exception Builder Builder
  deriving stock (Show)

-- | Transforms a list of 'CapturedOutput' into a lazy bytestring to be used
-- with golden tests.
capturedToBs :: [CapturedOutput] -> BSL.ByteString
capturedToBs =
  Builder.toLazyByteString
    . mconcat
    . L.intersperse "\n\n"
    . foldr go []
  where
    go (MonadTerminal title bs) acc = fmt "TERMINAL " title bs acc
    go (Logs title bs) acc = fmt "LOGS " title bs acc
    go (Exception title bs) acc = fmt "EXCEPTION " title bs acc
    fmt :: Builder -> Builder -> Builder -> [Builder] -> [Builder]
    fmt cons title bs acc =
      mconcat
        [ cons,
          title,
          "\n",
          bs
        ]
        : acc

-- | Runs safe-rm.
runSafeRm :: FilePath -> [String] -> IO ()
runSafeRm testDir = void . captureSafeRm testDir ""

-- | Runs safe-rm and captures terminal output.
captureSafeRm :: FilePath -> Builder -> [String] -> IO CapturedOutput
captureSafeRm testDir title = fmap (view _1) . captureSafeRmLogs testDir title

-- | Runs safe-rm and captures (terminal output, logs).
captureSafeRmLogs ::
  -- | Test dir. Used to strip dir from output to make paths deterministic.
  FilePath ->
  -- | Title to add to captured output.
  Builder ->
  -- Args.
  [String] ->
  IO (CapturedOutput, CapturedOutput)
captureSafeRmLogs testDir title argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  runFuncIO (Runner.runCmd cmd) env

  terminal <- replaceDir testDir <$> readIORef terminalRef
  logs <- replaceDir testDir <$> readIORef logsRef
  let terminalBs = Builder.byteString $ encodeUtf8 terminal
      logsBs = Builder.byteString $ encodeUtf8 logs

  pure (MonadTerminal title terminalBs, Logs title logsBs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs safe-rm and captures a thrown exception and logs.
captureSafeRmExceptionLogs ::
  forall e.
  Exception e =>
  -- | Test dir. Used to strip dir from output to make paths deterministic.
  FilePath ->
  -- | Title to add to captured output.
  Builder ->
  -- Args.
  [String] ->
  IO (CapturedOutput, CapturedOutput)
captureSafeRmExceptionLogs testDir title argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  result <- tryWithCallStack @e $ runFuncIO (Runner.runCmd cmd) env

  case result of
    Right _ ->
      error
        "captureSafeRmExceptionLogs: Expected exception, received none"
    Left ex -> do
      logs <- replaceDir testDir <$> readIORef logsRef
      let exceptionBs = exToBuilder testDir ex
          logsBs = txtToBuilder logs
      pure (Exception title exceptionBs, Logs title logsBs)
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

-- HACK: Our naive golden tests require exact string quality, which is a
-- problem since the full paths are non-deterministic, depending on the
-- environment. Here are some possible remedies:
--
-- 1. Don't use golden tests, or use the function that allows us to pass a
--    custom comparator.
--    R: Golden tests make updating the output extremely convenient, we're
--       not ready to give up on an easy diff.
-- 2. Use a typeclass to mock the directory so it can be deterministic.
--    R: The main problem here is that we need a _real_ path since we are
--       interacting with the actual filesystem. We would need to somehow
--       separate the "logged path" vs. the "used path" which sounds very
--       complicated.
-- 3. Search the output text for the non-deterministic path, and replace it
--    it with a fixed substitute.
--    R. This is something of a "hack", though it is simple and easy to
--       implement.
--
-- We currently use option 3.
replaceDir :: FilePath -> Text -> Text
replaceDir fp = T.replace (T.pack fp) "<dir>"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

txtToBuilder :: Text -> Builder
txtToBuilder = Builder.byteString . encodeUtf8

exToBuilder :: Exception e => FilePath -> e -> Builder
exToBuilder fp = txtToBuilder . replaceDir fp . T.pack . displayCallStack

-- | Fixes several fields on the CallStack that are either non-deterministic
-- (package name) or extremely brittle (line/col numbers). This eases testing.
fixCallStack :: CallStack -> CallStack
fixCallStack (PushCallStack a src cs) =
  PushCallStack a (fixSrcLoc src) (fixCallStack cs)
fixCallStack other = other

fixSrcLoc :: SrcLoc -> SrcLoc
fixSrcLoc loc =
  loc
    { srcLocPackage = "<package>",
      srcLocStartLine = 0,
      srcLocEndLine = 0,
      srcLocStartCol = 0,
      srcLocEndCol = 0
    }
