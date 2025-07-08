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
import Charon.Runner.Command
  ( DeleteParams (MkDeleteParams, paths, verbose),
    Force (MkForce),
    PermDeleteParams
      ( MkPermDeleteParams,
        prompt,
        strategy,
        verbose
      ),
    Prompt (MkPrompt),
    RestoreParams (MkRestoreParams, force, prompt, strategy, verbose),
    Verbose (MkVerbose),
  )
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LoggingConfig (MkLoggingConfig, logLevel, logSizeMode),
  )
import Charon.Runner.Config qualified as Config
import Charon.Runner.FileSizeMode (parseFileSizeMode)
import Charon.Runner.Phase (ConfigPhase (ConfigPhaseToml))
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

-- Command defaults!!!
-- delete, perm-delete, restore = verbose
-- maybe we can be clever and re-use e.g. restore params?

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
        verbose <- fmap MkVerbose <$> getFieldOptWith tomlDecoder "verbose"
        pure
          $ MkDeleteParams
            { paths = (),
              verbose
            }

      decodePermDeleteConfig :: Decoder (Maybe (PermDeleteParams ConfigPhaseToml))
      decodePermDeleteConfig = flip getFieldOptWith "perm-delete" $ do
        strategy <- getFieldOptWith @Bool tomlDecoder "indices"
        prompt <- fmap MkPrompt <$> getFieldOptWith tomlDecoder "prompt"
        verbose <- fmap MkVerbose <$> getFieldOptWith tomlDecoder "verbose"
        pure
          $ MkPermDeleteParams
            { prompt,
              strategy,
              verbose
            }

      decodeRestoreConfig :: Decoder (Maybe (RestoreParams ConfigPhaseToml))
      decodeRestoreConfig = flip getFieldOptWith "restore" $ do
        force <- fmap MkForce <$> getFieldOptWith tomlDecoder "force"
        strategy <- getFieldOptWith @Bool tomlDecoder "indices"
        prompt <- fmap MkPrompt <$> getFieldOptWith tomlDecoder "prompt"
        verbose <- fmap MkVerbose <$> getFieldOptWith tomlDecoder "verbose"
        pure
          $ MkRestoreParams
            { force,
              prompt,
              strategy,
              verbose
            }
