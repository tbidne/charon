{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides TOML configuration.
module Charon.Runner.Toml
  ( TomlConfig (..),
    defaultTomlConfig,
  )
where

import Charon.Data.Paths (RawPathI (MkRawPathI))
import Charon.Prelude
import Charon.Runner.Command.Delete
  ( DeleteParams
      ( MkDeleteParams,
        paths,
        prompt,
        verbose
      ),
  )
import Charon.Runner.Command.PermDelete
  ( PermDeleteParams
      ( MkPermDeleteParams,
        prompt,
        strategy,
        verbose
      ),
  )
import Charon.Runner.Command.Restore
  ( RestoreParams
      ( MkRestoreParams,
        force,
        prompt,
        strategy,
        verbose
      ),
  )
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LoggingConfig (MkLoggingConfig, logLevel, logSizeMode),
  )
import Charon.Runner.Config qualified as Config
import Charon.Runner.FileSizeMode (parseFileSizeMode)
import Charon.Runner.Phase (ConfigPhase (ConfigPhaseToml), parseSwitch)
import FileSystem.OsPath qualified as OsPath
import TOML
  ( DecodeTOML,
    getFieldOpt,
    getFieldOptWith,
  )
import TOML.Decode (Decoder, tomlDecoder)

-- | Holds TOML configuration.
data TomlConfig = MkTomlConfig
  { -- | Core config.
    coreConfig :: CoreConfig ConfigPhaseToml,
    deleteConfig :: Maybe (DeleteParams ConfigPhaseToml),
    permDeleteConfig :: Maybe (PermDeleteParams ConfigPhaseToml),
    restoreConfig :: Maybe (RestoreParams ConfigPhaseToml)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''TomlConfig

defaultTomlConfig :: TomlConfig
defaultTomlConfig =
  MkTomlConfig
    { coreConfig =
        MkCoreConfig
          { backend = Nothing,
            logging =
              MkLoggingConfig
                { logLevel = Nothing,
                  logSizeMode = Nothing
                },
            trashHome = Nothing
          },
      deleteConfig = Nothing,
      permDeleteConfig = Nothing,
      restoreConfig = Nothing
    }

instance DecodeTOML TomlConfig where
  tomlDecoder = do
    backend <- decodeBackend
    logLevel <- decodeLogLevel
    logSizeMode <- decodeSizeMode
    trashHome <- decodeTrashHome
    deleteConfig <- decodeDeleteConfig
    permDeleteConfig <- decodePermDeleteConfig
    restoreConfig <- decodeRestoreConfig
    pure
      $ MkTomlConfig
        { coreConfig =
            MkCoreConfig
              { backend,
                logging =
                  MkLoggingConfig
                    { logLevel,
                      logSizeMode
                    },
                trashHome
              },
          deleteConfig,
          permDeleteConfig,
          restoreConfig
        }
    where
      decodeTrashHome = do
        mh <- getFieldOpt "trash-home"
        case mh of
          Nothing -> pure Nothing
          Just h ->
            case OsPath.encodeValid h of
              Right p -> pure $ Just $ MkRawPathI p
              Left ex -> fail $ "Could not encode trash-home: " <> displayException ex
      decodeBackend = getFieldOptWith tomlDecoder "backend"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= Config.readLogLevel) "log-level"
      decodeSizeMode = getFieldOptWith (tomlDecoder >>= parseFileSizeMode) "log-size-mode"

      decodeDeleteConfig :: Decoder (Maybe (DeleteParams ConfigPhaseToml))
      decodeDeleteConfig = flip getFieldOptWith "delete" $ do
        prompt <- decodePrompt
        verbose <- decodeVerbose
        pure
          $ MkDeleteParams
            { paths = (),
              prompt,
              verbose
            }

      decodePermDeleteConfig :: Decoder (Maybe (PermDeleteParams ConfigPhaseToml))
      decodePermDeleteConfig = flip getFieldOptWith "perm-delete" $ do
        strategy <- decodeIndices
        prompt <- decodePrompt
        verbose <- decodeVerbose
        pure
          $ MkPermDeleteParams
            { prompt,
              strategy,
              verbose
            }

      decodeRestoreConfig :: Decoder (Maybe (RestoreParams ConfigPhaseToml))
      decodeRestoreConfig = flip getFieldOptWith "restore" $ do
        force <- decodeForce
        strategy <- decodeIndices
        prompt <- decodePrompt
        verbose <- decodeVerbose
        pure
          $ MkRestoreParams
            { force,
              prompt,
              strategy,
              verbose
            }

      decodeForce = getFieldOptWith tomlDecoder "force"
      decodeIndices = getFieldOptWith (tomlDecoder >>= parseSwitch) "indices"
      decodePrompt = getFieldOptWith tomlDecoder "prompt"
      decodeVerbose = getFieldOptWith tomlDecoder "verbose"
