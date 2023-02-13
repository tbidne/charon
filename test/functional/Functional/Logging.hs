{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests for logging.
--
-- @since 0.1
module Functional.Logging
  ( tests,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import Effects.LoggerNamespace
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter),
    Namespace,
  )
import Effects.LoggerNamespace qualified as Logger
import Functional.Prelude
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome)
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Env (Env)
import System.Environment qualified as SysEnv

data LoggerEnv = MkLoggerEnv
  { trashHome :: !(PathI TrashHome),
    logHandle :: !Handle,
    logFinalizer :: IO (),
    logLevel :: !LogLevel,
    logNamespace :: !Namespace,
    terminalRef :: !(IORef Text),
    charStream :: !(IORef CharStream)
  }

makeFieldLabelsNoPrefix ''LoggerEnv

deriving anyclass instance HasTrashHome LoggerEnv

instance MonadLogger (FuncIO LoggerEnv) where
  monadLoggerLog loc _src lvl msg = do
    handle <- asks (view #logHandle)
    logLevel <- asks (view #logLevel)
    when (logLevel <= lvl) $ do
      formatted <- Logger.formatLog (mkFormatter loc) lvl msg
      let bs = Logger.logStrToBs formatted
      liftIO $ BS.hPut handle bs
    where
      mkFormatter l =
        MkLogFormatter
          { newline = True,
            locStrategy = LocStable l,
            timezone = False
          }

instance MonadLoggerNamespace (FuncIO LoggerEnv) where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Logging"
    [ logging args,
      excludesMetadata args
    ]

logging :: IO FilePath -> TestTree
logging args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "logging"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      delArgList =
        ["-t", trashDir, "--log-level", "debug"]
          <> ("d" : filesToDelete)

  -- setup
  clearDirectory testDir
  createFiles filesToDelete
  assertFilesExist filesToDelete

  runLogging delArgList

  -- file assertions
  assertFilesExist [trashDir </> ".log"]
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete

  -- Test logging. In particular, test that startup does not fail when the
  -- log file does not already exist.
  replaceDirBS testDir <$> BS.readFile (trashDir </> ".log")
  where
    desc = "Logging is successful"
    gpath = goldenPath </> "logs.golden"

excludesMetadata :: IO FilePath -> TestTree
excludesMetadata args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "logging"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      delArgList =
        ["-t", trashDir, "--log-level", "debug"]
          <> ("d" : filesToDelete)
      listArgList =
        ["-t", trashDir, "--log-level", "debug", "l"]
      metadataArgList =
        ["-t", trashDir, "--log-level", "debug", "m"]

  -- setup
  clearDirectory testDir
  createFiles filesToDelete
  assertFilesExist filesToDelete

  runLogging delArgList
  runLogging listArgList
  runLogging metadataArgList

  replaceDirBS testDir <$> BS.readFile (trashDir </> ".log")
  where
    desc = "Log file is excluded from metadata entries count"
    gpath = goldenPath </> "log-metadata-excluded.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Logging"

transformEnv :: Env IO -> IO LoggerEnv
transformEnv env = do
  terminalRef <- newIORef ""
  charStream <- newIORef altAnswers
  (logHandle, logFinalizer, logLevel) <- case env ^. #logEnv % #logFile of
    Nothing -> error "no log env"
    Just lf -> pure (lf ^. #handle, lf ^. #finalizer, lf ^. #logLevel)
  pure $
    MkLoggerEnv
      { trashHome = env ^. #trashHome,
        logHandle,
        logFinalizer,
        logLevel,
        logNamespace = "logging-tests",
        terminalRef,
        charStream
      }

-- HACK: See the note on Functional.Prelude.unsafeReplaceDir. Note that we cannot
-- reuse that function directly since that operates on Text, whereas we have
-- to use ByteString here. Thus we implement the same idea here.
replaceDirBS :: FilePath -> ByteString -> BSL.ByteString
replaceDirBS fp =
  TLEnc.encodeUtf8
    . TL.fromStrict
    . fixRandomPaths
    . unsafeReplaceDir fp
    . TEnc.decodeUtf8With TEncError.lenientDecode

runLogging :: [String] -> IO ()
runLogging argList = bracket mkEnv closeLog run
  where
    mkEnv = do
      (env, cmd) <- SysEnv.withArgs argList Runner.getEnv
      (,cmd) <$> transformEnv env
    closeLog = view (_1 % #logFinalizer)
    run (loggerEnv, cmd) = runFuncIO (Runner.runCmd cmd) loggerEnv
