-- | Provides the 'CharonT' type for running Charon.
module Charon.Runner.CharonT
  ( CharonT (MkCharonT),
    runCharonT,
    usingCharonT,
  )
where

import Charon.Prelude
import Charon.Runner.Env (Env, LogFile)
import Effects.LoggerNS (defaultLogFormatter, guardLevel)
import Effects.LoggerNS qualified as Logger

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
      MonadThrow,
      MonadTime
    )
    via (ReaderT env m)

instance
  (MonadHandleWriter m, MonadTime m) =>
  MonadLogger (CharonT (Env m) m)
  where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#logEnv % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) -> do
        guardLevel logLevel lvl $ do
          formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
          let bs = Logger.logStrToBs formatted
          hPut handle bs
    where
      handleAndLevel :: Getter (LogFile m) (Handle, LogLevel)
      handleAndLevel =
        to (\lf -> bimap (view #handle) (view #logLevel) (lf, lf))

instance
  (MonadHandleWriter m, MonadTime m) =>
  MonadLoggerNS (CharonT (Env m) m)
  where
  getNamespace = asks (view (#logEnv % #logNamespace))
  localNamespace = local . over' (#logEnv % #logNamespace)

-- | Runs a 'CharonT' with the given @env@.
runCharonT :: CharonT env m a -> env -> m a
runCharonT (MkCharonT rdr) = runReaderT rdr

-- | Flipped 'runCharonT'
usingCharonT :: env -> CharonT env m a -> m a
usingCharonT = flip runCharonT
