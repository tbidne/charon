{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides concrete Env type for running SafeRm.
module SafeRm.Runner.Env
  ( Env (..),
    LogEnv (..),
    LogFile (..),
  )
where

import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasBackend, HasTrashHome)
import SafeRm.Prelude

-- | Data for file logging.
data LogFile = MkLogFile
  { -- | File handle.
    handle :: !Handle,
    -- | Level in which to log.
    logLevel :: !LogLevel
  }

instance Show LogFile where
  show (MkLogFile _ l) =
    "MkLogFile {handle = <handle>, logLevel ="
      <> show l
      <> "}"

makeFieldLabelsNoPrefix ''LogFile

-- | Holds logging env data.
data LogEnv = MkLogEnv
  { -- | Data for file logging.
    logFile :: !(Maybe LogFile),
    -- | The current logging namespace.
    logNamespace :: !Namespace
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''LogEnv

-- | Concrete environment type that can be used for running SafeRm
-- functions.
data Env = MkEnv
  { -- | Trash home.
    trashHome :: !(PathI TrashHome),
    -- | Backend.
    backend :: !Backend,
    -- | The logging environment.
    logEnv :: !LogEnv
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Env

deriving anyclass instance HasTrashHome Env

deriving anyclass instance HasBackend Env
