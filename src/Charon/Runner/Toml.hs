{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides TOML configuration.
module Charon.Runner.Toml
  ( TomlConfig (..),
    -- mergeConfigs,
    defaultTomlConfig,
  )
where

import Charon.Data.Paths (RawPathI (MkRawPathI))
import Charon.Prelude
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LoggingConfig (MkLoggingConfig, logLevel, logSizeMode),
  )
import Charon.Runner.Config qualified as Config
import Charon.Runner.FileSizeMode (parseFileSizeMode)
import Charon.Runner.Phase (ConfigPhase (ConfigPhaseToml))
import FileSystem.OsPath qualified as OsPath
import TOML
  ( DecodeTOML (),
    getFieldOpt,
    getFieldOptWith,
  )
import TOML.Decode (tomlDecoder)

-- | Holds TOML configuration.
newtype TomlConfig = MkTomlConfig
  { -- | Core config.
    coreConfig :: CoreConfig ConfigPhaseToml
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
          }
    }

instance DecodeTOML TomlConfig where
  tomlDecoder = do
    backend <- decodeBackend
    logLevel <- decodeLogLevel
    logSizeMode <- decodeSizeMode
    trashHome <- decodeTrashHome
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
              }
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
