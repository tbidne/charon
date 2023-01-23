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
import SafeRm.Runner.Args (Args, CommandArg (..), _ListArg)
import SafeRm.Runner.Command
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
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | List command configuration.
    --
    -- @since 0.1
    listCommand :: !(Maybe CmdListCfg)
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
      <*> getFieldOptWith tomlDecoder "list"
    where
      decodeTrashHome = fmap MkPathI <$> getFieldOpt "trash-home"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= U.readLogLevel) "log-level"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
--
-- Also updates the args' command with possible toml configuration.
--
-- @since 0.1
mergeConfigs :: Args -> TomlConfig -> (TomlConfig, Command)
mergeConfigs args toml = (mergedConfig, newCmd)
  where
    cmd = args ^. #command
    mergedConfig =
      MkTomlConfig
        { trashHome = U.mergeAlt #trashHome #trashHome args toml,
          logLevel = U.mergeAlt #logLevel #logLevel args toml,
          -- Get the list config from the args, if it exists, and then
          -- combine it with the toml config
          listCommand = args ^? (#command % _ListArg) <> toml ^. #listCommand
        }
    newCmd = cmdFromToml mergedConfig cmd

-- Returns the new command after possibly updating the old command from the
-- toml configuration.
cmdFromToml :: TomlConfig -> CommandArg -> Command
cmdFromToml toml = \case
  -- simple translations
  DeleteArg paths -> Delete paths
  DeletePermArg b paths -> DeletePerm b paths
  EmptyArg b -> Empty b
  RestoreArg paths -> Restore paths
  MetadataArg -> Metadata
  -- NOTE: The toml param contains config for the following explicitly listed
  -- commands. For these, use the toml rather than the command as it will
  -- have the most up-to-date config data (merged args + toml)
  (ListArg _) ->
    case toml ^. #listCommand of
      -- HACK: This Nothing -> mempty case is needed for the types to work,
      -- but it is technically never used and should be removed.
      --
      -- If we are here then listCommand is definitely Just, as toml is the
      -- "merged config" thus getting it from the Args at the very least.
      --
      -- Is there a less fragile way to do this?
      Nothing -> List mempty
      Just cfg -> List $ listCfgToCmd cfg

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
      -- default to SinglelineAuto
      Nothing -> FormatTabularAuto
