{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides merged configuration.
module Charon.Runner.Merged
  ( MergedConfig (..),
    mergeConfigs,
  )
where

import Charon.Backend.Data (Backend (BackendCbor, BackendFdo))
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
    fromRaw,
  )
import Charon.Prelude
import Charon.Runner.Args (Args)
import Charon.Runner.Command (Command, mergeCommand)
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LogLevelConfig (LogLevelOn),
    LoggingConfig (MkLoggingConfig, logLevel, logSizeMode),
  )
import Charon.Runner.FileSizeMode qualified as FileSizeMode
import Charon.Runner.Phase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
  )
import Charon.Runner.Toml (TomlConfig)
import Charon.Utils qualified as U
import Effects.FileSystem.PathReader (getXdgData)

data MergedConfig = MkMergedConfig
  { command :: Command ConfigPhaseMerged,
    coreConfig :: CoreConfig ConfigPhaseMerged
  }

makeFieldLabelsNoPrefix ''MergedConfig

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
mergeConfigs ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  Args ->
  TomlConfig ->
  m MergedConfig
mergeConfigs args toml = do
  command <- mergeCommand (args ^. #command)

  let backend =
        fromMaybe
          BackendCbor
          (U.mergeAlt #backend #backend argsCore tomlCore)

      logging =
        mergeLogging (argsCore ^. #logging) (tomlCore ^. #logging)

  let thRawPath = U.mergeAlt #trashHome #trashHome argsCore tomlCore
  thPath <- traverse fromRaw thRawPath

  trashHome <- trashOrDefault backend thPath

  pure
    $ MkMergedConfig
      { command,
        coreConfig =
          MkCoreConfig
            { backend,
              logging,
              trashHome
            }
      }
  where
    argsCore = args ^. #coreConfig
    tomlCore = toml ^. #coreConfig

    mergeLogging ::
      LoggingConfig ConfigPhaseArgs ->
      LoggingConfig ConfigPhaseToml ->
      Maybe (LoggingConfig ConfigPhaseMerged)
    mergeLogging argsLogging tomlLogging =
      case argsLogging ^. #logLevel <|> tomlLogging ^. #logLevel of
        Just l@(LogLevelOn _) -> Just $ mkLogging l
        _ -> Nothing
      where
        mkLogging logLevel =
          MkLoggingConfig
            { logLevel,
              logSizeMode =
                fromMaybe
                  FileSizeMode.defaultSizeMode
                  (argsLogging ^. #logSizeMode <|> tomlLogging ^. #logSizeMode)
            }

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
trashOrDefault ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Backend ->
  Maybe (PathI TrashHome) ->
  m (PathI TrashHome)
-- 1. Use explicit path.
trashOrDefault _ (Just th) = pure th
-- 2. No path but fdo: default fdo location.
trashOrDefault BackendFdo Nothing = getFdoDefaultTrashHome
-- 3. Generic default.
trashOrDefault _ _ = getGenericDefaultTrashHome

-- | Retrieves the default trash directory.
getGenericDefaultTrashHome ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  m (PathI TrashHome)
getGenericDefaultTrashHome = MkPathI <$> (getXdgData charonPath)

-- | Retrieves the default trash directory for fdo backend.
getFdoDefaultTrashHome ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  m (PathI TrashHome)
getFdoDefaultTrashHome = MkPathI <$> (getXdgData [osp|Trash|])
