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

import SafeRm.Data.PathData
  ( PathDataFormat
      ( FormatMultiline,
        FormatTabular,
        FormatTabularAuto
      ),
  )
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Prelude
import SafeRm.Runner.Args (Args, CommandArg (..))
import SafeRm.Runner.Command (Command (..), ListCommand (..))
import SafeRm.Runner.Config (CmdListCfg (..), ListFormatCfg (..))
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
    logLevel :: !(Maybe (Maybe LogLevel))
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
defaultTomlConfig = MkTomlConfig Nothing Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTrashHome
      <*> decodeLogLevel
    where
      decodeTrashHome = fmap MkPathI <$> getFieldOpt "trash-home"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= U.readLogLevel) "log-level"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
--
-- @since 0.1
mergeConfigs :: Args -> TomlConfig -> (TomlConfig, Command)
mergeConfigs args toml = (mergedConfig, mkCommand cmd)
  where
    cmd = args ^. #command
    mergedConfig =
      MkTomlConfig
        { trashHome = U.mergeAlt #trashHome #trashHome args toml,
          logLevel = U.mergeAlt #logLevel #logLevel args toml
        }

-- Returns the new command after possibly updating the old command from the
-- toml configuration.
mkCommand :: CommandArg -> Command
mkCommand = \case
  -- simple translations
  DeleteArg paths -> Delete paths
  DeletePermArg force paths -> DeletePerm force paths
  EmptyArg b -> Empty b
  RestoreArg paths -> Restore paths
  MetadataArg -> Metadata
  (ListArg cfg) -> List $ listCfgToCmd cfg

listCfgToCmd :: CmdListCfg -> ListCommand
listCfgToCmd listCfg = do
  MkListCommand
    { format,
      sort,
      revSort
    }
  where
    sort = U.fromMaybeMonoid (listCfg ^. #sort)
    revSort = fromMaybe False (listCfg ^. #revSort)
    format = case listCfg ^. #format of
      Just FormatMultilineCfg -> FormatMultiline
      Just FormatTabularCfg ->
        FormatTabular
          (fromMaybe 10 (listCfg ^. #nameTrunc))
          (fromMaybe 22 (listCfg ^. #origTrunc))
      Just FormatAutoCfg -> FormatTabularAuto
      -- default to FormatTabularAuto
      Nothing -> FormatTabularAuto
