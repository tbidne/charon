{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for running safe-rm.
module SafeRm.Runner
  ( -- * Main functions
    runSafeRm,
    runCmd,

    -- * Helpers
    getConfiguration,
  )
where

import Data.Bytes (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes qualified as Bytes
import Data.Text qualified as T
import Effectful.FileSystem.HandleWriter.Dynamic (withBinaryFile)
import Effectful.FileSystem.PathReader.Dynamic (getXdgData)
import SafeRm qualified
import SafeRm.Data.Backend (Backend (BackendCbor))
import SafeRm.Data.Index (Sort)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData.Formatting (PathDataFormat)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
  )
import SafeRm.Env (HasBackend, HasTrashHome)
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
import SafeRm.Runner.Command
  ( Command
      ( Convert,
        Delete,
        Empty,
        List,
        Merge,
        Metadata,
        PermDelete,
        Restore
      ),
    CommandP2,
  )
import SafeRm.Runner.Env
  ( Env (MkEnv, backend, trashHome),
    LogEnv (MkLogEnv),
    LogFile (MkLogFile),
    handle,
    logEnv,
    logFile,
    logLevel,
    logNamespace,
  )
import SafeRm.Runner.FileSizeMode (FileSizeMode (..))
import SafeRm.Runner.FileSizeMode qualified as FileSizeMode
import SafeRm.Runner.Logging (runLoggerDynamic, runLoggerNSDynamic)
import SafeRm.Runner.Toml (TomlConfig, defaultTomlConfig, mergeConfigs)
import SafeRm.Utils qualified as U
import TOML qualified

-- | Entry point for running SafeRm. Does everything: reads CLI args,
-- optional Toml config, and creates the environment before running
-- SafeRm.
runSafeRm ::
  ( Concurrent :> es,
    FileReaderDynamic :> es,
    FileWriterDynamic :> es,
    HandleWriterDynamic :> es,
    IORefStatic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    PosixCompatStatic :> es,
    TerminalDynamic :> es,
    TimeDynamic :> es
  ) =>
  Eff es ()
runSafeRm = do
  (config, cmd) <- getConfiguration
  withEnv config $ \env ->
    runReader env
      $ runLoggerNSDynamic
      $ runLoggerDynamic
      $ runCmd @Env cmd

-- | Runs SafeRm in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runSafeRm', when we want to use a
-- custom env.
runCmd ::
  forall env es.
  ( HasBackend env,
    HasTrashHome env,
    Concurrent :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    FileReaderDynamic :> es,
    FileWriterDynamic :> es,
    HandleWriterDynamic :> es,
    IORefStatic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    PosixCompatStatic :> es,
    Reader env :> es,
    TerminalDynamic :> es,
    TimeDynamic :> es
  ) =>
  CommandP2 ->
  Eff es ()
runCmd cmd =
  -- NOTE: This adds a callstack to any thrown exceptions e.g. exitFailure.
  -- This is what we want, as it similar to what we will get once GHC
  -- natively supports exceptions with callstacks.
  runCmd' cmd `catchAny` logEx
  where
    runCmd' = \case
      Delete paths -> SafeRm.delete @env paths
      PermDelete force paths -> SafeRm.permDelete @env force paths
      Empty force -> SafeRm.emptyTrash @env force
      Restore paths -> SafeRm.restore @env paths
      List listCmd ->
        printIndex @env (listCmd ^. #format) (listCmd ^. #sort) (listCmd ^. #revSort)
      Metadata -> printMetadata @env
      Convert dest -> SafeRm.convert @env dest
      Merge dest -> SafeRm.merge @env dest

    logEx :: (HasCallStack) => SomeException -> Eff es a
    logEx ex = do
      $(logError) (T.pack $ displayException ex)
      throwM ex

withEnv ::
  ( HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    TerminalDynamic :> es
  ) =>
  TomlConfig ->
  (Env -> Eff es a) ->
  Eff es a
withEnv mergedConfig onEnv = do
  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome

  withLogHandle (mergedConfig ^. #logSizeMode) $ \handle ->
    let logFile =
          join (mergedConfig ^. #logLevel) <&> \logLevel ->
            MkLogFile
              { handle,
                logLevel
              }
     in onEnv
          $ MkEnv
            { trashHome,
              backend = fromMaybe BackendCbor (mergedConfig ^. #backend),
              logEnv =
                MkLogEnv
                  { logFile,
                    logNamespace = "main"
                  }
            }

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the user
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
getConfiguration ::
  ( FileReaderDynamic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es
  ) =>
  Eff es (TomlConfig, CommandP2)
getConfiguration = do
  -- get CLI args
  args <- getArgs

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    TomlPath tomlPath -> readConfig tomlPath
    -- no toml config path given...
    TomlDefault -> do
      xdgConfig <- getXdgConfig safeRmPath
      let defPath = xdgConfig </> [osp|config.toml|]
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
        Left tomlErr -> throwM tomlErr

printIndex ::
  forall env es.
  ( HasBackend env,
    HasTrashHome env,
    Concurrent :> es,
    FileReaderDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PosixCompatStatic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  PathDataFormat ->
  Sort ->
  Bool ->
  Eff es ()
printIndex style sort revSort = do
  index <- SafeRm.getIndex @env
  formatted <- Index.formatIndex @env style sort revSort index
  putTextLn formatted

printMetadata ::
  forall env es.
  ( HasBackend env,
    HasTrashHome env,
    FileReaderDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Eff es ()
printMetadata = SafeRm.getMetadata @env >>= prettyDel

prettyDel :: (Pretty a, TerminalDynamic :> es) => a -> Eff es ()
prettyDel = putTextLn . U.renderPretty

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
trashOrDefault ::
  ( PathReaderDynamic :> es
  ) =>
  Maybe (PathI TrashHome) ->
  Eff es (PathI TrashHome)
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
getTrashHome :: (PathReaderDynamic :> es) => Eff es (PathI TrashHome)
getTrashHome = MkPathI <$> (getXdgData safeRmPath)

withLogHandle ::
  ( HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    TerminalDynamic :> es
  ) =>
  Maybe FileSizeMode ->
  (Handle -> Eff es a) ->
  Eff es a
withLogHandle sizeMode onHandle = do
  xdgState <- getXdgState safeRmPath
  createDirectoryIfMissing True xdgState

  MkPathI logPath <- Env.getTrashLog

  handleLogSize logPath sizeMode

  withBinaryFile logPath AppendMode onHandle

handleLogSize ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    TerminalDynamic :> es
  ) =>
  OsPath ->
  Maybe FileSizeMode ->
  Eff es ()
handleLogSize logFile msizeMode = do
  logExists <- doesFileExist logFile
  when logExists $ do
    logSize <- getFileSize logFile
    let logSize' = MkBytes (fromIntegral logSize)

    case sizeMode of
      FileSizeModeWarn warnSize ->
        when (logSize' > warnSize)
          $ putTextLn
          $ sizeWarning warnSize logFile logSize'
      FileSizeModeDelete delSize ->
        when (logSize' > delSize) $ do
          putTextLn $ sizeWarning delSize logFile logSize' <> " Deleting log."
          removeFile logFile
  where
    sizeMode = fromMaybe FileSizeMode.defaultSizeMode msizeMode
    sizeWarning warnSize fp fileSize =
      mconcat
        [ "Warning: log dir ",
          decodeOsToFpShowText fp,
          " has size: ",
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

safeRmPath :: OsPath
safeRmPath = [osp|safe-rm|]
