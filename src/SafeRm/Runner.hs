{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( -- * Main functions
    runSafeRm,
    runCmd,

    -- * Helpers
    getEnv,
    getConfiguration,
  )
where

import Data.Bytes (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes qualified as Bytes
import Data.Text qualified as T
import Effects.FileSystem.PathReader (getXdgData, getXdgState)
import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))
import SafeRm qualified
import SafeRm.Data.Index (Sort)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (PathDataFormat)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
  )
import SafeRm.Env (HasTrashHome)
import SafeRm.Env qualified as Env
import SafeRm.Prelude
import SafeRm.Runner.Args
  ( TomlConfigPath
      ( TomlDefault,
        TomlNone,
        TomlPath
      ),
    getArgs,
  )
import SafeRm.Runner.Command (Command (..), CommandP2)
import SafeRm.Runner.Env
  ( Env (MkEnv, trashHome),
    LogEnv (MkLogEnv),
    LogFile (MkLogFile),
    finalizer,
    handle,
    logEnv,
    logFile,
    logLevel,
    logNamespace,
  )
import SafeRm.Runner.FileSizeMode (FileSizeMode (..))
import SafeRm.Runner.FileSizeMode qualified as FileSizeMode
import SafeRm.Runner.SafeRmT (runSafeRmT)
import SafeRm.Runner.Toml (TomlConfig, defaultTomlConfig, mergeConfigs)
import SafeRm.Utils qualified as U
import TOML qualified

-- | Entry point for running SafeRm. Does everything: reads CLI args,
-- optional Toml config, and creates the environment before running
-- SafeRm.
--
-- @since 0.1
runSafeRm ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  m ()
runSafeRm = do
  (config, cmd) <- getConfiguration

  bracket
    (configToEnv config)
    closeLogging
    (runSafeRmT (runCmd cmd))
  where
    closeLogging :: (Monad m) => Env m -> m ()
    closeLogging env = do
      let mFinalizer = env ^? #logEnv % #logFile %? #finalizer
      fromMaybe (pure ()) mFinalizer

-- | Runs SafeRm in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runSafeRm', when we want to use a
-- custom env.
runCmd ::
  forall m env.
  ( HasCallStack,
    HasTrashHome env,
    MonadLoggerNS m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CommandP2 ->
  m ()
runCmd cmd =
  -- NOTE: This adds a callstack to any thrown exceptions e.g. exitFailure.
  -- This is what we want, as it similar to what we will get once GHC
  -- natively supports exceptions with callstacks.
  runCmd' cmd `catchCS` logEx
  where
    runCmd' = \case
      Delete paths -> SafeRm.delete paths
      DeletePerm force paths -> SafeRm.deletePermanently force paths
      Empty force -> SafeRm.emptyTrash force
      Restore paths -> SafeRm.restore paths
      List listCmd -> do
        printIndex (listCmd ^. #format) (listCmd ^. #sort) (listCmd ^. #revSort)
        putStrLn ""
        printMetadata
      Metadata -> printMetadata

    logEx :: (HasCallStack) => SomeException -> m a
    logEx ex = do
      $(logError) (T.pack $ displayNoCS ex)
      throwCS ex

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the final Env used
-- by SafeRm.
--
-- @since 0.1
getEnv ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  m (Env m, CommandP2)
getEnv = do
  (mergedConfig, command) <- getConfiguration

  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome

  logFile <- case join (mergedConfig ^. #logLevel) of
    Nothing -> pure Nothing
    Just lvl -> do
      h <- getLogHandle (mergedConfig ^. #logSizeMode)
      pure $
        Just $
          MkLogFile
            { handle = h,
              logLevel = lvl,
              finalizer = hFlush h `finally` hClose h
            }
  let env =
        MkEnv
          { trashHome,
            logEnv =
              MkLogEnv
                { logFile,
                  logNamespace = "runner"
                }
          }
  pure (env, command)

configToEnv ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  TomlConfig ->
  m (Env m)
configToEnv mergedConfig = do
  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome

  logFile <- case join (mergedConfig ^. #logLevel) of
    Nothing -> pure Nothing
    Just lvl -> do
      h <- getLogHandle (mergedConfig ^. #logSizeMode)
      pure $
        Just $
          MkLogFile
            { handle = h,
              logLevel = lvl,
              finalizer = hFlush h `finally` hClose h
            }
  pure $
    MkEnv
      { trashHome,
        logEnv =
          MkLogEnv
            { logFile,
              logNamespace = "runner"
            }
      }

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the user
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
--
-- @since 0.1
getConfiguration ::
  ( HasCallStack,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  m (TomlConfig, CommandP2)
getConfiguration = do
  -- get CLI args
  args <- getArgs

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    TomlPath tomlPath -> readConfig tomlPath
    -- no toml config path given...
    TomlDefault -> do
      xdgConfig <- getXdgConfig "safe-rm"
      let defPath = xdgConfig </> "config.toml"
      exists <- doesFileExist defPath
      if exists
        then -- 2. config exists at default path: read
          readConfig defPath
        else -- 3. no config exists: return default (empty)
          pure defaultTomlConfig
    -- 4. toml explicitly disabled
    TomlNone -> pure defaultTomlConfig

  -- merge shared CLI and toml values
  pure $ mergeConfigs args tomlConfig
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwCS tomlErr

printIndex ::
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  PathDataFormat ->
  Sort ->
  Bool ->
  m ()
printIndex style sort revSort = do
  index <- SafeRm.getIndex
  formatted <- Index.formatIndex style sort revSort index
  putTextLn formatted

printMetadata ::
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  m ()
printMetadata = SafeRm.getMetadata >>= prettyDel

prettyDel :: (Pretty a, MonadTerminal m) => a -> m ()
prettyDel = putTextLn . U.renderPretty

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
--
-- @since 0.1
trashOrDefault ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Maybe (PathI TrashHome) ->
  m (PathI TrashHome)
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
--
-- @since 0.1
getTrashHome ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  m (PathI TrashHome)
getTrashHome = MkPathI <$> (getXdgData "safe-rm")

getLogHandle ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  Maybe FileSizeMode ->
  m Handle
getLogHandle sizeMode = do
  xdgState <- getXdgState "safe-rm"
  createDirectoryIfMissing True xdgState

  MkPathI logPath <- Env.getTrashLog

  handleLogSize logPath sizeMode

  openBinaryFile logPath AppendMode

handleLogSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  Path ->
  Maybe FileSizeMode ->
  m ()
handleLogSize logFile msizeMode = do
  logExists <- doesFileExist logFile
  when logExists $ do
    logSize <- getFileSize logFile
    let logSize' = MkBytes (fromIntegral logSize)

    case sizeMode of
      FileSizeModeWarn warnSize ->
        when (logSize' > warnSize) $
          putTextLn $
            sizeWarning warnSize logFile logSize'
      FileSizeModeDelete delSize ->
        when (logSize' > delSize) $ do
          putTextLn $ sizeWarning delSize logFile logSize' <> " Deleting log."
          removeFile logFile
  where
    sizeMode = fromMaybe FileSizeMode.defaultSizeMode msizeMode
    sizeWarning warnSize fp fileSize =
      mconcat
        [ "Warning: log dir '",
          T.pack fp,
          "' has size: ",
          formatBytes fileSize,
          ", but specified threshold is: ",
          formatBytes warnSize,
          "."
        ]

    formatBytes =
      Bytes.formatSized (MkFloatingFormatter (Just 2)) Bytes.sizedFormatterNatural
        . Bytes.normalize
        -- Convert to double _before_ normalizing. We may lose some precision
        -- here, but it is better than normalizing a natural, which will
        -- truncate (i.e. greater precision loss).
        . fmap (fromIntegral @Natural @Double)
