{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Runner.Config
  ( CoreConfig (..),
    LoggingConfig (..),
    LogLevelConfig (..),
    _LogLevelOff,
    _LogLevelOn,
    LogEnv (..),
    LogFile (..),
    readLogLevel,
    logLevelStrings,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Data.Paths (PathI, PathIndex (TrashHome), RawPathI)
import Charon.Prelude
import Charon.Runner.FileSizeMode (FileSizeMode)
import Charon.Runner.Phase
import Data.Text qualified as T

type TrashHomeF :: ConfigPhase -> Type
type family TrashHomeF p where
  TrashHomeF ConfigPhaseArgs = Maybe (RawPathI TrashHome)
  TrashHomeF ConfigPhaseToml = Maybe (RawPathI TrashHome)
  TrashHomeF ConfigPhaseMerged = PathI TrashHome
  TrashHomeF ConfigPhaseEnv = PathI TrashHome

data LogLevelConfig
  = LogLevelOff
  | LogLevelOn LogLevel
  deriving stock (Eq, Show)

makePrisms ''LogLevelConfig

data LoggingConfig p = MkLoggingConfig
  { logLevel :: ConfigPhaseF p LogLevelConfig,
    logSizeMode :: ConfigPhaseF p FileSizeMode
  }

deriving stock instance
  ( Show (ConfigPhaseF p FileSizeMode),
    Show (ConfigPhaseF p LogLevelConfig)
  ) =>
  Show (LoggingConfig p)

deriving stock instance
  ( Eq (ConfigPhaseF p FileSizeMode),
    Eq (ConfigPhaseF p LogLevelConfig)
  ) =>
  Eq (LoggingConfig p)

makeFieldLabelsNoPrefix ''LoggingConfig

-- | Data for file logging.
data LogFile = MkLogFile
  { -- | File handle.
    handle :: Handle,
    -- | Level in which to log.
    logLevel :: LogLevel
  }

makeFieldLabelsNoPrefix ''LogFile

-- | Holds logging env data.
data LogEnv = MkLogEnv
  { -- | Data for file logging.
    logFile :: !(Maybe LogFile),
    -- | The current logging namespace.
    logNamespace :: Namespace
  }

makeFieldLabelsNoPrefix ''LogEnv

type LoggingF :: ConfigPhase -> Type
type family LoggingF p where
  -- Args and Toml are Maybes in both fields
  LoggingF ConfigPhaseArgs = LoggingConfig ConfigPhaseArgs
  LoggingF ConfigPhaseToml = LoggingConfig ConfigPhaseToml
  -- Merged is either the entire config or Nothing
  LoggingF ConfigPhaseMerged = Maybe (LoggingConfig ConfigPhaseMerged)
  -- Env is the actual file if we use it.
  LoggingF ConfigPhaseEnv = LogEnv

type CoreConfig :: ConfigPhase -> Type
data CoreConfig p = MkCoreConfig
  { backend :: ConfigPhaseF p Backend,
    logging :: LoggingF p,
    trashHome :: TrashHomeF p
  }

makeFieldLabelsNoPrefix ''CoreConfig

deriving stock instance
  ( Show (ConfigPhaseF p Backend),
    Show (LoggingF p),
    Show (TrashHomeF p)
  ) =>
  Show (CoreConfig p)

deriving stock instance
  ( Eq (ConfigPhaseF p Backend),
    Eq (LoggingF p),
    Eq (TrashHomeF p)
  ) =>
  Eq (CoreConfig p)

readLogLevel :: (MonadFail m) => Text -> m LogLevelConfig
readLogLevel = \case
  "none" -> pure LogLevelOff
  "debug" -> pure $ LogLevelOn LevelDebug
  "info" -> pure $ LogLevelOn LevelInfo
  "warn" -> pure $ LogLevelOn LevelWarn
  "error" -> pure $ LogLevelOn LevelError
  "fatal" -> pure $ LogLevelOn levelFatal
  other ->
    fail
      $ mconcat
        [ "Expected log-level ",
          logLevelStrings,
          ", received: ",
          T.unpack other
        ]

-- | String description of possible log levels parsed by 'readLogLevel'.
logLevelStrings :: String
logLevelStrings = "(debug|info|warn|error|fatal|none)"
