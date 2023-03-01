{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides TOML configuration.
--
-- @since 0.1
module SafeRm.Runner.Toml
  ( TomlConfig (..),
    mergeConfigs,
    defaultTomlConfig,
  )
where

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
--
-- @since 0.1
data TomlConfig = MkTomlConfig
  { -- | Trash home.
    --
    -- @since 0.1
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | Log level.
    --
    -- @since 0.1
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | Whether to warn/delete large log files.
    --
    -- @since 0.1
    logSizeMode :: !(Maybe FileSizeMode)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''TomlConfig

-- | @since 0.1
defaultTomlConfig :: TomlConfig
defaultTomlConfig = MkTomlConfig Nothing Nothing Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTrashHome
      <*> decodeLogLevel
      <*> decodeSizeMode
    where
      decodeTrashHome = fmap MkPathI <$> getFieldOpt "trash-home"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= U.readLogLevel) "log-level"
      decodeSizeMode = getFieldOptWith (tomlDecoder >>= parseFileSizeMode) "log-size-mode"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
--
-- @since 0.1
mergeConfigs :: Args -> TomlConfig -> (TomlConfig, CommandP2)
mergeConfigs args toml = (mergedConfig, advancePhase cmd)
  where
    cmd = args ^. #command
    mergedConfig =
      MkTomlConfig
        { trashHome = U.mergeAlt #trashHome #trashHome args toml,
          logLevel = U.mergeAlt #logLevel #logLevel args toml,
          logSizeMode = U.mergeAlt #logSizeMode #logSizeMode args toml
        }
