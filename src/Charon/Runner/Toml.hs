{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides TOML configuration.
module Charon.Runner.Toml
  ( TomlConfig (..),
    TomlConfigP1,
    TomlConfigP2,
    mergeConfigs,
    defaultTomlConfig,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Data.Paths
  ( PathIndex (TrashHome),
    RawPathI (MkRawPathI),
    fromRaw,
  )
import Charon.Prelude
import Charon.Runner.Args (Args)
import Charon.Runner.Command (CmdPathF, CommandP2, advancePhaseCmd)
import Charon.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import Charon.Runner.Phase (Phase (Phase1, Phase2))
import Charon.Utils qualified as U
import FileSystem.OsPath qualified as OsPath
import TOML
  ( DecodeTOML (),
    getFieldOpt,
    getFieldOptWith,
  )
import TOML.Decode (tomlDecoder)

-- | Holds TOML configuration.
data TomlConfig s = MkTomlConfig
  { -- | Trash home.
    trashHome :: !(Maybe (CmdPathF s TrashHome)),
    -- | Backend.
    backend :: Maybe Backend,
    -- | Log level. The double Maybe is so we distinguish between
    -- unspecified (Nothing) and explicitly disabled (Just Nothing).
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | Whether to warn/delete large log files.
    logSizeMode :: Maybe FileSizeMode
  }

type TomlConfigP1 = TomlConfig Phase1

type TomlConfigP2 = TomlConfig Phase2

deriving stock instance (Eq (CmdPathF s TrashHome)) => Eq (TomlConfig s)

deriving stock instance (Show (CmdPathF s TrashHome)) => Show (TomlConfig s)

makeFieldLabelsNoPrefix ''TomlConfig

defaultTomlConfig :: TomlConfig Phase1
defaultTomlConfig = MkTomlConfig Nothing Nothing Nothing Nothing

instance DecodeTOML (TomlConfig Phase1) where
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
            case OsPath.encodeValid h of
              Right p -> pure $ Just $ MkRawPathI p
              Left ex -> fail $ "Could not encode trash-home: " <> displayException ex
      decodeBackend = getFieldOptWith tomlDecoder "backend"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= U.readLogLevel) "log-level"
      decodeSizeMode = getFieldOptWith (tomlDecoder >>= parseFileSizeMode) "log-size-mode"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
mergeConfigs ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  Args ->
  TomlConfigP1 ->
  m (TomlConfigP2, CommandP2)
mergeConfigs args toml = do
  cmd2 <- advancePhaseCmd cmd

  let thRawPath = U.mergeAlt #trashHome #trashHome args toml
  thPath <- traverse fromRaw thRawPath

  pure (mkMergedConfig thPath, cmd2)
  where
    cmd = args ^. #command
    mkMergedConfig trashHome =
      MkTomlConfig
        { trashHome,
          backend = U.mergeAlt #backend #backend args toml,
          logLevel = U.mergeAlt #logLevel #logLevel args toml,
          logSizeMode = U.mergeAlt #logSizeMode #logSizeMode args toml
        }
