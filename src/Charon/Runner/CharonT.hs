{-# LANGUAGE CPP #-}

-- | Provides the 'CharonT' type for running Charon.
module Charon.Runner.CharonT
  ( CharonT (MkCharonT),
    runCharonT,
    usingCharonT,
  )
where

import Charon.Prelude
import Charon.Runner.Config (LogFile)
import Charon.Runner.Env (Env)
import Effects.Logger (guardLevel)
import Effects.Logger.Namespace qualified as NS

-- | `CharonT` is the main application type that runs shell commands.
type CharonT :: Type -> (Type -> Type) -> Type -> Type
newtype CharonT env m a = MkCharonT (ReaderT env m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadPathReader,
      MonadPathWriter,
      MonadPosixCompat,
      MonadReader env,
      MonadTerminal,
      MonadThread,
      MonadThrow,
      MonadTime
    )
    via (ReaderT env m)

#if !WINDOWS
deriving newtype instance (MonadPosix m) => MonadPosix (CharonT env m)
#endif

deriving newtype instance (MonadHaskeline m) => MonadHaskeline (CharonT env m)

instance
  (MonadHandleWriter m, MonadThread m, MonadTime m) =>
  MonadLogger (CharonT Env m)
  where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#coreConfig % #logging % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) -> do
        guardLevel logLevel lvl $ do
          formatted <- NS.formatLog (NS.defaultLogFormatter loc) lvl msg
          let bs = NS.logStrToBs formatted
          hPut handle bs
    where
      handleAndLevel :: Getter LogFile (Handle, LogLevel)
      handleAndLevel =
        to (\lf -> bimap (view #handle) (view #logLevel) (lf, lf))

-- | Runs a 'CharonT' with the given @env@.
runCharonT :: CharonT env m a -> env -> m a
runCharonT (MkCharonT rdr) = runReaderT rdr

-- | Flipped 'runCharonT'
usingCharonT :: env -> CharonT env m a -> m a
usingCharonT = flip runCharonT
