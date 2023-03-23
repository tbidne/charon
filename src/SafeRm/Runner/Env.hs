{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides concrete Env type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.Env
  ( Env (..),
    LogEnv (..),
    LogFile (..),
  )
where

import Effects.LoggerNS (Namespace)
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome)
import SafeRm.Prelude

-- | Data for file logging.
--
-- @since 0.1
data LogFile m = MkLogFile
  { -- | File handle.
    --
    -- @since 0.1
    handle :: !Handle,
    -- | Level in which to log.
    --
    -- @since 0.1
    logLevel :: !LogLevel
  }

-- | @since 0.1
instance Show (LogFile m) where
  show (MkLogFile _ l) =
    "MkLogFile {handle = <handle>, logLevel ="
      <> show l
      <> "}"

-- | @since 0.1
makeFieldLabelsNoPrefix ''LogFile

-- | Holds logging env data.
--
-- @since 0.1
data LogEnv m = MkLogEnv
  { -- | Data for file logging.
    --
    -- @since 0.1
    logFile :: !(Maybe (LogFile m)),
    -- | The current logging namespace.
    --
    -- @since 0.1
    logNamespace :: !Namespace
  }
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''LogEnv

-- | Concrete environment type that can be used for running SafeRm
-- functions.
--
-- @since 0.1
data Env m = MkEnv
  { -- | Trash home.
    --
    -- @since 0.1
    trashHome :: !(PathI TrashHome),
    -- | The logging environment.
    --
    -- @since 0.1
    logEnv :: !(LogEnv m)
  }
  deriving stock
    ( -- | @since 0.1
      Show
    )

makeFieldLabelsNoPrefix ''Env

-- | @since 0.1
deriving anyclass instance HasTrashHome (Env m)
