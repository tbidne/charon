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

import Data.Text qualified as T
import Effects.MonadTerminal (putTextLn)
import SafeRm qualified
import SafeRm.Data.Index (Sort)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (PathDataFormat)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Env (HasTrashHome)
import SafeRm.Exception (TomlDecodeE (MkTomlDecodeE))
import SafeRm.Prelude
import SafeRm.Runner.Args
  ( TomlConfigPath
      ( TomlDefault,
        TomlNone,
        TomlPath
      ),
    getArgs,
  )
import SafeRm.Runner.Command
  ( Command
      ( Delete,
        DeletePerm,
        Empty,
        List,
        Metadata,
        Restore
      ),
  )
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
import SafeRm.Runner.SafeRmT (runSafeRmT)
import SafeRm.Runner.Toml (TomlConfig, mergeConfigs)
import TOML qualified

-- | Entry point for running SafeRm. Does everything: reads CLI args,
-- optional Toml config, and creates the environment before running
-- SafeRm.
--
-- @since 0.1
runSafeRm ::
  ( MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIO m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    HasCallStack,
    MonadCallStack m,
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
    closeLogging env = do
      let mFinalizer = env ^? #logEnv % #logFile %? #finalizer
      liftIO $ addCallStack $ fromMaybe (pure ()) mFinalizer

-- | Runs SafeRm in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runSafeRm', when we want to use a
-- custom env.
runCmd ::
  ( HasCallStack,
    HasTrashHome env,
    MonadCallStack m,
    MonadLoggerNamespace m,
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
  Command ->
  m ()
runCmd cmd = runCmd' cmd `catchAnyNoCS` logEx
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

    logEx ex = do
      $(logError) (T.pack $ displayCallStack ex)
      throwIO ex

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the final Env used
-- by SafeRm.
--
-- @since 0.1
getEnv ::
  ( HasCallStack,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIO m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadCallStack m
  ) =>
  m (Env, Command)
getEnv = do
  (mergedConfig, command) <- getConfiguration

  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome
  -- NOTE: Needed so below openFile command does not fail
  Paths.applyPathI (createDirectoryIfMissing False) trashHome

  logFile <- case join (mergedConfig ^. #logLevel) of
    Nothing -> pure Nothing
    Just lvl -> do
      let logPath = trashHome ^. #unPathI </> ".log"
      h <- openBinaryFile logPath AppendMode
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
    MonadCallStack m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  TomlConfig ->
  m Env
configToEnv mergedConfig = do
  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome
  -- NOTE: Needed so below openFile command does not fail
  Paths.applyPathI (createDirectoryIfMissing False) trashHome

  logFile <- case join (mergedConfig ^. #logLevel) of
    Nothing -> pure Nothing
    Just lvl -> do
      let logPath = trashHome ^. #unPathI </> ".log"
      h <- openBinaryFile logPath AppendMode
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
    MonadIO m,
    MonadPathReader m,
    MonadCallStack m
  ) =>
  m (TomlConfig, Command)
getConfiguration = do
  -- get CLI args
  args <- liftIO getArgs

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
          pure mempty
    -- 4. toml explicitly disabled
    TomlNone -> pure mempty

  -- merge shared CLI and toml values
  pure $ mergeConfigs args tomlConfig
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwWithCallStack $ MkTomlDecodeE tomlErr

printIndex ::
  ( HasCallStack,
    HasTrashHome env,
    MonadCallStack m,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNamespace m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  PathDataFormat ->
  Sort ->
  Bool ->
  m ()
printIndex style sort revSort =
  SafeRm.getIndex
    >>= putTextLn
      . Index.formatIndex style sort revSort

printMetadata ::
  ( MonadPathReader m,
    HasCallStack,
    HasTrashHome env,
    MonadCallStack m,
    MonadFileReader m,
    MonadLoggerNamespace m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m ()
printMetadata = SafeRm.getMetadata >>= prettyDel

prettyDel :: (Pretty a, MonadTerminal m) => a -> m ()
prettyDel =
  putTextLn
    . renderStrict
    . layoutCompact
    . pretty

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
getTrashHome = MkPathI . (</> ".trash") <$> getHomeDirectory
