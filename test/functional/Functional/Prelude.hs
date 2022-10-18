{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for functional test suite.
--
-- @since 0.1
module Functional.Prelude
  ( module X,

    -- * Types
    FuncEnv (..),

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
    fixPackage,
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time (LocalTime (LocalTime))
import Data.Time.LocalTime (midday)
import GHC.Stack.Types
  ( CallStack (PushCallStack),
    SrcLoc (srcLocPackage),
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Effects.MonadCallStack (MonadCallStack (getCallStack))
import SafeRm.Effects.MonadFsReader
  ( MonadFsReader
      ( canonicalizePath,
        doesDirectoryExist,
        doesFileExist,
        doesPathExist,
        getFileSize,
        listDirectory,
        readFile
      ),
  )
import SafeRm.Effects.MonadFsWriter (MonadFsWriter)
import SafeRm.Effects.MonadLoggerContext
  ( MonadLoggerContext (getNamespace, localNamespace),
    Namespace,
  )
import SafeRm.Effects.MonadLoggerContext qualified as Logger
import SafeRm.Effects.MonadSystemTime (MonadSystemTime (getSystemTime), Timestamp (MkTimestamp))
import SafeRm.Effects.MonadTerminal (MonadTerminal (putStr, putStrLn))
import SafeRm.Env (HasTrashHome)
import SafeRm.FileUtils as X
import SafeRm.Prelude as X
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
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
import UnliftIO.Environment qualified as SysEnv

-- | Environment for running functional tests.
data FuncEnv = MkFuncEnv
  { -- | Trash home.
    trashHome :: !(PathI TrashHome),
    -- | Log namespace.
    logNamespace :: !Namespace,
    -- | Saves the terminal output.
    terminalRef :: !(IORef Text),
    -- | Saves the logs output.
    logsRef :: !(IORef Text)
  }

makeFieldLabelsNoPrefix ''FuncEnv

deriving anyclass instance HasTrashHome FuncEnv

-- | Type for running functional tests.
newtype FuncIO a = MkFuncIO (ReaderT FuncEnv IO a)
  deriving
    ( Applicative,
      MonadFsWriter,
      Functor,
      Monad,
      MonadIO,
      MonadReader FuncEnv,
      MonadUnliftIO
    )
    via (ReaderT FuncEnv IO)

instance MonadFsReader FuncIO where
  getFileSize = const (pure $ afromInteger 5)
  readFile = liftIO . readFile
  doesFileExist = liftIO . doesFileExist
  doesDirectoryExist = liftIO . doesDirectoryExist
  doesPathExist = liftIO . doesPathExist
  canonicalizePath = liftIO . canonicalizePath
  listDirectory = liftIO . listDirectory

instance MonadCallStack FuncIO where
  getCallStack = pure $ fixPackage ?callStack

instance MonadTerminal FuncIO where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  putStrLn = putStr

instance MonadSystemTime FuncIO where
  getSystemTime = pure $ MkTimestamp localTime
    where
      localTime = LocalTime (toEnum 59_000) midday

instance MonadLogger FuncIO where
  monadLoggerLog _loc _src lvl msg = do
    formatted <- Logger.formatLogNoLoc True lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (view #logsRef)
    modifyIORef' logsRef (<> txt)

instance MonadLoggerContext FuncIO where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

runFuncIO :: FuncIO a -> FuncEnv -> IO a
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
capturedToBs = Builder.toLazyByteString . foldr go ""
  where
    go (MonadTerminal title bs) acc = fmt "TERMINAL " title bs acc
    go (Logs title bs) acc = fmt "LOGS " title bs acc
    go (Exception title bs) acc = fmt "EXCEPTION " title bs acc
    fmt cons title bs acc =
      mconcat
        [ cons,
          title,
          "\n",
          bs,
          "\n\n",
          acc
        ]

-- | Runs safe-rm.
runSafeRm :: FilePath -> [String] -> IO ()
runSafeRm testDir = void . captureSafeRm testDir ""

-- | Runs safe-rm and captures terminal output.
captureSafeRm :: FilePath -> Builder -> [String] -> IO CapturedOutput
captureSafeRm testDir title = fmap (view _1) . captureSafeRmLogs testDir title

-- | Runs safe-rm and captures (terminal output, logs).
captureSafeRmLogs ::
  FilePath ->
  Builder ->
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
  let terminalBs = Builder.byteString $ TEnc.encodeUtf8 terminal
      logsBs = Builder.byteString $ TEnc.encodeUtf8 logs

  pure (MonadTerminal title terminalBs, Logs title logsBs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs safe-rm and captures a thrown exception and logs.
captureSafeRmExceptionLogs ::
  forall e.
  Exception e =>
  FilePath ->
  Builder ->
  [String] ->
  IO (CapturedOutput, CapturedOutput)
captureSafeRmExceptionLogs testDir title argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  result <- try @_ @e $ runFuncIO (Runner.runCmd cmd) env

  case result of
    Right _ ->
      throwString
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
  pure $
    MkFuncEnv
      { trashHome = trashHome,
        terminalRef,
        logsRef,
        logNamespace = "functional"
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
txtToBuilder = Builder.byteString . TEnc.encodeUtf8

exToBuilder :: Exception e => FilePath -> e -> Builder
exToBuilder fp = txtToBuilder . replaceDir fp . T.pack . displayException

fixPackage :: CallStack -> CallStack
fixPackage (PushCallStack a src cs) =
  PushCallStack a (fixSrcPackage src) (fixPackage cs)
fixPackage other = other

fixSrcPackage :: SrcLoc -> SrcLoc
fixSrcPackage = set' #srcLocPackage "<package>"
