{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides concrete Env type for running Charon.
module Charon.Runner.Env
  ( Env (..),
    withEnv,
  )
where

import Charon.Data.Paths (PathI (MkPathI))
import Charon.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import Charon.Env qualified as Env
import Charon.Prelude
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LogEnv
      ( MkLogEnv,
        logFile,
        logNamespace
      ),
    LogFile (MkLogFile),
    LogLevelConfig (LogLevelOff, LogLevelOn),
  )
import Charon.Runner.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeWarn
      ),
  )
import Charon.Runner.Merged (MergedConfig)
import Charon.Runner.Phase
  ( ConfigPhase
      ( ConfigPhaseEnv
      ),
  )
import Data.Bytes (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes qualified as Bytes
import Effects.FileSystem.HandleWriter (MonadHandleWriter (withBinaryFile))
import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))

-- | Concrete environment type that can be used for running Charon
-- functions.
newtype Env = MkEnv
  { coreConfig :: CoreConfig ConfigPhaseEnv
  }

makeFieldLabelsNoPrefix ''Env

-- Logging requires a Getter/Setter 'namespace' optic on the env type.
instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
  LabelOptic "namespace" k Env Env x y
  where
  labelOptic =
    lensVL $ \f (MkEnv a1) ->
      fmap
        (\b -> MkEnv (set' (#logging % #logNamespace) b a1))
        (f (a1 ^. (#logging % #logNamespace)))

instance HasBackend Env where
  getBackend = view (#coreConfig % #backend)

instance HasTrashHome Env where
  getTrashHome = view (#coreConfig % #trashHome)

withEnv ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  MergedConfig ->
  (Env -> m a) ->
  m a
withEnv cfg onEnv =
  withLogEnv $ \logging ->
    onEnv
      $ MkEnv
        { coreConfig =
            MkCoreConfig
              { backend = cfg ^. (#coreConfig % #backend),
                logging,
                trashHome = cfg ^. (#coreConfig % #trashHome)
              }
        }
  where
    withLogEnv :: (LogEnv -> m a) -> m a
    withLogEnv onLogEnv =
      withLogFile $ \logFile -> do
        onLogEnv
          $ MkLogEnv
            { logFile,
              logNamespace = "main"
            }

    withLogFile :: (Maybe LogFile -> m a) -> m a
    withLogFile onMLogFile =
      case cfg ^. (#coreConfig % #logging) of
        Nothing -> onMLogFile Nothing
        Just logging -> case logging ^. #logLevel of
          LogLevelOff -> onMLogFile Nothing
          LogLevelOn lvl ->
            withLogHandle
              (logging ^. #logSizeMode)
              (\h -> onMLogFile $ Just $ MkLogFile h lvl)

withLogHandle ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  FileSizeMode ->
  (Handle -> m a) ->
  m a
withLogHandle sizeMode onHandle = do
  xdgState <- getXdgState charonPath
  createDirectoryIfMissing True xdgState

  MkPathI logPath <- Env.getTrashLog

  handleLogSize logPath sizeMode

  withBinaryFile logPath AppendMode onHandle

handleLogSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  OsPath ->
  FileSizeMode ->
  m ()
handleLogSize logFile sizeMode = do
  logExists <- doesFileExist logFile
  when logExists $ do
    logSize <- getFileSize logFile
    let logSize' = MkBytes (fromIntegral logSize)

    case sizeMode of
      FileSizeModeWarn warnSize ->
        when (logSize' > warnSize)
          $ putTextLn
          $ sizeWarning warnSize logFile logSize'
      FileSizeModeDelete delSize ->
        when (logSize' > delSize) $ do
          putTextLn $ sizeWarning delSize logFile logSize' <> " Deleting log."
          removeFile logFile
  where
    sizeWarning warnSize fp fileSize =
      mconcat
        [ "Warning: log dir ",
          decodeDisplayExT fp,
          " has size: ",
          formatBytes fileSize,
          ", but specified threshold is: ",
          formatBytes warnSize,
          "."
        ]

    formatBytes =
      Bytes.formatSized (MkFloatingFormatter (Just 2)) Bytes.sizedFormatterNatural
        . Bytes.normalize
        -- Convert to double _before_ normalizing. We may lose some precision
        -- here, but it is better than normalizing a natural, which will
        -- truncate (i.e. greater precision loss).
        . fmap (fromIntegral @Natural @Double)
