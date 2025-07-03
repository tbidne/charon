{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides concrete Env type for running Charon.
module Charon.Runner.Env
  ( Env (..),
    LogEnv (..),
    LogFile (..),
  )
where

import Charon.Backend.Data (Backend)
import Charon.Data.Paths (PathI, PathIndex (TrashHome))
import Charon.Env (HasBackend, HasTrashHome)
import Charon.Prelude

-- | Data for file logging.
data LogFile m = MkLogFile
  { -- | File handle.
    handle :: Handle,
    -- | Level in which to log.
    logLevel :: LogLevel
  }

instance Show (LogFile m) where
  show (MkLogFile _ l) =
    "MkLogFile {handle = <handle>, logLevel ="
      <> show l
      <> "}"

makeFieldLabelsNoPrefix ''LogFile

-- | Holds logging env data.
data LogEnv m = MkLogEnv
  { -- | Data for file logging.
    logFile :: !(Maybe (LogFile m)),
    -- | The current logging namespace.
    logNamespace :: Namespace
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''LogEnv

-- | Concrete environment type that can be used for running Charon
-- functions.
data Env m = MkEnv
  { -- | Trash home.
    trashHome :: PathI TrashHome,
    -- | Backend.
    backend :: Backend,
    -- | The logging environment.
    logEnv :: LogEnv m
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Env

-- Logging requires a Getter/Setter 'namespace' optic on the env type.
instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
  LabelOptic "namespace" k (Env m) (Env m) x y
  where
  labelOptic =
    lensVL $ \f (MkEnv a1 a2 a3) ->
      fmap
        (\b -> MkEnv a1 a2 (set' #logNamespace b a3))
        (f (a3 ^. #logNamespace))

deriving anyclass instance HasTrashHome (Env m)

deriving anyclass instance HasBackend (Env m)
