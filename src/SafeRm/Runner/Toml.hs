{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides TOML configuration.
module SafeRm.Runner.Toml
  ( TomlConfig (..),
    mergeConfigs,
    defaultTomlConfig,
  )
where

import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Prelude
import SafeRm.Runner.Args (Args)
import SafeRm.Runner.Command (CommandP2)
import SafeRm.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import SafeRm.Runner.Phase (advancePhase)
import SafeRm.Utils qualified as U
import TOML
  ( DecodeTOML (..),
    getFieldOpt,
    getFieldOptWith,
  )

-- | Holds TOML configuration.
data TomlConfig = MkTomlConfig
  { -- | Trash home.
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | Backend.
    backend :: !(Maybe Backend),
    -- | Log level. The double Maybe is so we distinguish between
    -- unspecified (Nothing) and explicitly disabled (Just Nothing).
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | Whether to warn/delete large log files.
    logSizeMode :: !(Maybe FileSizeMode)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''TomlConfig

defaultTomlConfig :: TomlConfig
defaultTomlConfig = MkTomlConfig Nothing Nothing Nothing Nothing

instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTrashHome
      <*> decodeBackend
      <*> decodeLogLevel
      <*> decodeSizeMode
    where
      decodeTrashHome = do
        mh <- getFieldOpt "trash-home"
        case mh of
          Nothing -> pure Nothing
          Just h ->
            case encodeFpToOs h of
              Right p -> pure $ Just $ MkPathI p
              Left ex -> fail $ "Could not encode trash-home: " <> displayException ex
      decodeBackend = getFieldOptWith tomlDecoder "backend"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= U.readLogLevel) "log-level"
      decodeSizeMode = getFieldOptWith (tomlDecoder >>= parseFileSizeMode) "log-size-mode"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
mergeConfigs :: Args -> TomlConfig -> (TomlConfig, CommandP2)
mergeConfigs args toml = (mergedConfig, advancePhase cmd)
  where
    cmd = args ^. #command
    mergedConfig =
      MkTomlConfig
        { trashHome = U.mergeAlt #trashHome #trashHome args toml,
          backend = U.mergeAlt #backend #backend args toml,
          logLevel = U.mergeAlt #logLevel #logLevel args toml,
          logSizeMode = U.mergeAlt #logSizeMode #logSizeMode args toml
        }
