{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import Charon.Data.Index qualified as Index
import Charon.Env (HasBackend, HasTrashHome)
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
  )
import Charon.Runner.Command.List (ListParams)
import Charon.Runner.Env qualified as Env
import Charon.Runner.Merged
import Charon.Runner.Merged qualified as Merged
import Charon.Runner.Phase
import Charon.Runner.Toml (defaultTomlConfig)
import Charon.Utils qualified as U
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
    MonadHaskeline m,
    MonadIORef m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
runCharon = do
  merged <- getConfiguration
  Env.withEnv merged (runCharonT $ runCmd $ merged ^. #command)

-- | Runs Charon in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runCharon', when we want to use a
-- custom env.
runCmd ::
  forall m env k.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadLoggerNS m env k,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadHaskeline m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Command ConfigPhaseMerged ->
  m ()
runCmd cmd =
  -- NOTE: This adds a callstack to any thrown exceptions e.g. exitFailure.
  -- This is what we want, as it similar to what we will get once GHC
  -- natively supports exceptions with callstacks.
  runCmd' cmd `catch` logEx
  where
    runCmd' = \case
      Delete params -> Charon.delete params
      PermDelete params -> Charon.permDelete params
      Empty prompt -> Charon.emptyTrash prompt
      Restore params -> Charon.restore params
      List listCmd -> printIndex listCmd
      Metadata -> printMetadata
      Convert dest -> Charon.convert dest
      Merge dest -> Charon.merge dest

    logEx :: (HasCallStack) => SomeException -> m a
    logEx ex = do
      $(logError) (U.displayExT ex)
      throwM ex

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the user
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
getConfiguration ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  m MergedConfig
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
  Merged.mergeConfigs args tomlConfig
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwM tomlErr

printIndex ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  ListParams ConfigPhaseMerged ->
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
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m ()
printMetadata = Charon.getMetadata >>= prettyDel

prettyDel :: (Display a, MonadTerminal m) => a -> m ()
prettyDel = putTextLn . U.renderPretty
