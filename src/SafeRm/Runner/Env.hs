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

import Effects.LoggerNamespace (Namespace)
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome)
import SafeRm.Prelude

-- | Data for file logging.
--
-- @since 0.1
data LogFile = MkLogFile
  { -- | File handle.
    --
    -- @since 0.1
    handle :: !Handle,
    -- | Level in which to log.
    --
    -- @since 0.1
    logLevel :: !LogLevel,
    -- Finalizer to run e.g. flush/close.
    --
    -- @since 0.1
    finalizer :: IO ()
  }

makeFieldLabelsNoPrefix ''LogFile

-- | Holds logging env data.
--
-- @since 0.1
data LogEnv = MkLogEnv
  { -- | Data for file logging.
    --
    -- @since 0.1
    logFile :: !(Maybe LogFile),
    -- | The current logging namespace.
    --
    -- @since 0.1
    logNamespace :: !Namespace
  }

makeFieldLabelsNoPrefix ''LogEnv

-- | Concrete environment type that can be used for running SafeRm
-- functions.
--
-- @since 0.1
data Env = MkEnv
  { -- | Trash home.
    --
    -- @since 0.1
    trashHome :: !(PathI TrashHome),
    -- | The logging environment.
    --
    -- @since 0.1
    logEnv :: !LogEnv
  }

makeFieldLabelsNoPrefix ''Env

-- | @since 0.1
deriving anyclass instance HasTrashHome Env
