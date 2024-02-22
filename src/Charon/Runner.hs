{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for running charon.
module Charon.Runner
  ( -- * Main functions
    runCharon,
    runCmd,

    -- * Helpers
    getConfiguration,
  )
where

import Charon qualified
import Charon.Backend.Data (Backend (BackendCbor))
import Charon.Data.Index qualified as Index
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
  )
import Charon.Env (HasBackend, HasTrashHome)
import Charon.Env qualified as Env
import Charon.Prelude
import Charon.Runner.Args
  ( TomlConfigPath
      ( TomlDefault,
        TomlNone,
        TomlPath
      ),
    getArgs,
  )
import Charon.Runner.CharonT (runCharonT)
import Charon.Runner.Command
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
import Charon.Runner.Command.List (ListCmdP2)
import Charon.Runner.Env
  ( Env (MkEnv, backend, logEnv, trashHome),
    LogEnv (MkLogEnv, logFile, logNamespace),
    LogFile (MkLogFile, handle, logLevel),
  )
import Charon.Runner.FileSizeMode (FileSizeMode (..))
import Charon.Runner.FileSizeMode qualified as FileSizeMode
import Charon.Runner.Toml (TomlConfig, defaultTomlConfig, mergeConfigs)
import Charon.Utils qualified as U
import Data.Bytes (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes qualified as Bytes
import Effects.FileSystem.HandleWriter (withBinaryFile)
import Effects.FileSystem.PathReader (getXdgData, getXdgState)
import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))
import TOML qualified

-- | Entry point for running Charon. Does everything: reads CLI args,
-- optional Toml config, and creates the environment before running
-- Charon.
runCharon ::
  ( HasCallStack,
    MonadAsync m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadTime m
  ) =>
  m ()
runCharon = do
  (config, cmd) <- getConfiguration
  withEnv config (runCharonT $ runCmd cmd)

-- | Runs Charon in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runCharon', when we want to use a
-- custom env.
runCmd ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
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
      Delete paths -> Charon.delete paths
      PermDelete force paths -> Charon.permDelete force paths
      Empty force -> Charon.emptyTrash force
      Restore paths -> Charon.restore paths
      List listCmd -> printIndex listCmd
      Metadata -> printMetadata
      Convert dest -> Charon.convert dest
      Merge dest -> Charon.merge dest

    logEx :: (HasCallStack) => SomeException -> m a
    logEx ex = do
      $(logError) (U.displayExT ex)
      throwCS ex

withEnv ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  TomlConfig ->
  (Env m -> m a) ->
  m a
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
      xdgConfig <- getXdgConfig charonPath
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
        Left tomlErr -> throwCS tomlErr

printIndex ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  ListCmdP2 ->
  m ()
printIndex listCmd = do
  index <- Charon.getIndex
  formatted <- Index.formatIndex listCmd index
  putTextLn formatted

printMetadata ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m ()
printMetadata = Charon.getMetadata >>= prettyDel

prettyDel :: (Display a, MonadTerminal m) => a -> m ()
prettyDel = putTextLn . U.renderPretty

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
trashOrDefault ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Maybe (PathI TrashHome) ->
  m (PathI TrashHome)
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
getTrashHome ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  m (PathI TrashHome)
getTrashHome = MkPathI <$> (getXdgData charonPath)

withLogHandle ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  Maybe FileSizeMode ->
  (Handle -> m a) ->
  m a
withLogHandle sizeMode onHandle = do
  xdgState <- getXdgState charonPath
  createDirectoryIfMissing True xdgState

  MkPathI logPath <- Env.getTrashLog

  handleLogSize logPath sizeMode

  withBinaryFile logPath AppendMode onHandle

handleLogSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  OsPath ->
  Maybe FileSizeMode ->
  m ()
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
          decodeOsToFpDisplayExT fp,
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

charonPath :: OsPath
charonPath = [osp|charon|]
