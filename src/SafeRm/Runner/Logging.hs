-- | Provides logging handlers
module SafeRm.Runner.Logging
  ( runLoggerDynamic,
    runLoggerNSDynamic,
  )
where

import Effectful.Logger.Dynamic (LoggerDynamic (LoggerLog))
import Effectful.LoggerNS.Dynamic
  ( LoggerNSDynamic (GetNamespace, LocalNamespace),
    defaultLogFormatter,
  )
import Effectful.LoggerNS.Dynamic qualified as Logger
import SafeRm.Prelude
import SafeRm.Runner.Env (Env, LogFile, handle, logLevel)

-- | LoggerDynamic handler.
runLoggerDynamic ::
  forall es a.
  ( LoggerNSDynamic :> es,
    HandleWriterDynamic :> es,
    Reader Env :> es,
    TimeDynamic :> es
  ) =>
  Eff (LoggerDynamic : es) a ->
  Eff es a
runLoggerDynamic = interpret $ \_ -> \case
  LoggerLog loc _src lvl msg -> do
    mhandle <- asks @Env (preview (#logEnv % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) ->
        when (logLevel <= lvl) $ do
          formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
          let bs = Logger.logStrToBs formatted
          hPut handle bs
    where
      handleAndLevel :: Lens' LogFile (Handle, LogLevel)
      handleAndLevel =
        lens
          (\lf -> bimap (view #handle) (view #logLevel) (lf, lf))
          (\lf (h, ll) -> lf {logLevel = ll, handle = h})

-- | Runs 'LoggerNSDynamic'.
runLoggerNSDynamic ::
  forall es a.
  ( Reader Env :> es
  ) =>
  Eff (LoggerNSDynamic : es) a ->
  Eff es a
runLoggerNSDynamic = interpret $ \env -> \case
  GetNamespace -> asks @Env (view (#logEnv % #logNamespace))
  LocalNamespace f m -> localSeqUnlift env $ \runner ->
    local @Env (over' (#logEnv % #logNamespace) f) (runner m)
