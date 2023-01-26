-- | Provides the 'SafeRmT' type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.SafeRmT
  ( SafeRmT (MkSafeRmT),
    runSafeRmT,
    usingSafeRmT,
  )
where

import Effects.LoggerNamespace (defaultLogFormatter)
import Effects.LoggerNamespace qualified as Logger
import SafeRm.Prelude
import SafeRm.Runner.Env (Env, LogFile, handle, logLevel)

-- | `SafeRmT` is the main application type that runs shell commands.
--
-- @since 0.1
type SafeRmT :: Type -> (Type -> Type) -> Type -> Type
newtype SafeRmT env m a = MkSafeRmT (ReaderT env m a)
  deriving
    ( -- | @since 0.1
      Functor,
      -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      MonadCatch,
      -- | @since 0.1
      MonadFileReader,
      -- | @since 0.1
      MonadFileWriter,
      -- | @since 0.1
      MonadHandleWriter,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      MonadIORef,
      -- | @since 0.1
      MonadPathReader,
      -- | @since 0.1
      MonadPathSize,
      -- | @since 0.1
      MonadPathWriter,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadTerminal,
      -- | @since 0.1
      MonadThrow,
      -- | @since 0.1
      MonadTime
    )
    via (ReaderT env m)

-- | @since 0.1
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

-- | @since 0.1
instance
  (MonadHandleWriter m, MonadTime m) =>
  MonadLoggerNamespace (SafeRmT (Env m) m)
  where
  getNamespace = asks (view (#logEnv % #logNamespace))
  localNamespace = local . over' (#logEnv % #logNamespace)

-- | Runs a 'SafeRmT' with the given @env@.
--
-- @since 0.1
runSafeRmT :: SafeRmT env m a -> env -> m a
runSafeRmT (MkSafeRmT rdr) = runReaderT rdr

-- | Flipped 'runSafeRmT'
--
-- @since 0.1
usingSafeRmT :: env -> SafeRmT env m a -> m a
usingSafeRmT = flip runSafeRmT
