-- | Provides the 'SafeRmT' type for running SafeRm.
module SafeRm.Runner.SafeRmT
  ( SafeRmT (MkSafeRmT),
    runSafeRmT,
    usingSafeRmT,
  )
where

import Effects.LoggerNS (defaultLogFormatter)
import Effects.LoggerNS qualified as Logger
import SafeRm.Prelude
import SafeRm.Runner.Env (Env, LogFile, handle, logLevel)

-- | `SafeRmT` is the main application type that runs shell commands.
type SafeRmT :: Type -> (Type -> Type) -> Type -> Type
newtype SafeRmT env m a = MkSafeRmT (ReaderT env m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadPathReader,
      MonadPathSize,
      MonadPathWriter,
      MonadReader env,
      MonadTerminal,
      MonadThrow,
      MonadTime
    )
    via (ReaderT env m)

instance
  (MonadHandleWriter m, MonadTime m) =>
  MonadLogger (SafeRmT (Env m) m)
  where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#logEnv % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) ->
        when (logLevel <= lvl) $ do
          formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
          let bs = Logger.logStrToBs formatted
          hPut handle bs
    where
      handleAndLevel :: Lens' (LogFile m) (Handle, LogLevel)
      handleAndLevel =
        lens
          (\lf -> bimap (view #handle) (view #logLevel) (lf, lf))
          (\lf (h, ll) -> lf {logLevel = ll, handle = h})

instance
  (MonadHandleWriter m, MonadTime m) =>
  MonadLoggerNS (SafeRmT (Env m) m)
  where
  getNamespace = asks (view (#logEnv % #logNamespace))
  localNamespace = local . over' (#logEnv % #logNamespace)

-- | Runs a 'SafeRmT' with the given @env@.
runSafeRmT :: SafeRmT env m a -> env -> m a
runSafeRmT (MkSafeRmT rdr) = runReaderT rdr

-- | Flipped 'runSafeRmT'
usingSafeRmT :: env -> SafeRmT env m a -> m a
usingSafeRmT = flip runSafeRmT
