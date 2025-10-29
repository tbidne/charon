{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This modules provides an executable for running charon.
module Charon.Runner
  ( -- * Main functions
    runCharon,
    runCmd,

    -- * Helpers
    getConfiguration,
  )
where

import Charon qualified
import Charon.Data.Index qualified as Index
import Charon.Env (HasBackend, HasTrashHome)
import Charon.Prelude
import Charon.Runner.Args
  ( TomlConfigPath
      ( TomlDefault,
        TomlNone,
        TomlPath
      ),
    getArgs,
  )
import Charon.Runner.CharonT (runCharonT)
import Charon.Runner.Command
  ( Command
      ( Convert,
        Delete,
        Empty,
        List,
        Merge,
        Metadata,
        PermDelete,
        Restore
      ),
  )
import Charon.Runner.Command.List (ListParams)
import Charon.Runner.Env qualified as Env
import Charon.Runner.Merged
import Charon.Runner.Merged qualified as Merged
import Charon.Runner.Phase
import Charon.Runner.Toml (defaultTomlConfig)
import Charon.Utils qualified as U
import Data.Foldable qualified as F
import Data.Set qualified as Set
import Data.Text qualified as T
import Effects.FileSystem.PathWriter qualified as PW
import TOML qualified

-- | Entry point for running Charon. Does everything: reads CLI args,
-- optional Toml config, and creates the environment before running
-- Charon.
runCharon ::
  ( HasCallStack,
    MonadAsync m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadHaskeline m,
    MonadIORef m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixFilesC m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
runCharon = do
  config@(_, merged) <- getConfiguration
  Env.withEnv merged (runCharonT $ runCmd config)

-- | Runs Charon in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runCharon', when we want to use a
-- custom env.
runCmd ::
  forall m env k.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadLoggerNS m env k,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadHaskeline m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixFilesC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  (OsPath, MergedConfig) ->
  m ()
runCmd (xdgState, config) = do
  -- NOTE: This adds a callstack to any thrown exceptions e.g. exitFailure.
  -- This is what we want, as it similar to what we will get once GHC
  -- natively supports exceptions with callstacks.
  eResult <- trySync (runCmd' cmd)
  saveTrashEntryCompletions xdgState eResult
  case eResult of
    Right _ -> pure ()
    Left ex -> logEx ex
  where
    runCmd' = \case
      Delete params -> Charon.delete params
      PermDelete params -> Charon.permDelete params
      Empty prompt -> Charon.emptyTrash prompt
      Restore params -> Charon.restore params
      List listCmd -> printIndex listCmd
      Metadata -> printMetadata
      Convert dest -> Charon.convert dest
      Merge dest -> Charon.merge dest

    logEx :: (HasCallStack) => SomeException -> m a
    logEx ex = do
      $(logError) (U.displayExT ex)
      -- If we are going to die with an exception, print a newline so any
      -- prior output is separated from the error message, for clarity.
      putStrLn ""
      throwM ex

    cmd = config ^. #command

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the user
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
getConfiguration ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  m (OsPath, MergedConfig)
getConfiguration = do
  xdgState <- getXdgStateCharon
  completions <- readTrashEntryCompletions xdgState

  -- get CLI args
  args <- getArgs completions

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    TomlPath tomlPath -> readConfig tomlPath
    -- no toml config path given...
    TomlDefault -> do
      xdgConfig <- getXdgConfig charonPath
      let defPath = xdgConfig </> [osp|config.toml|]
      exists <- doesFileExist defPath
      if exists
        then -- 2. config exists at default path: read
          readConfig defPath
        else -- 3. no config exists: return default (empty)
          pure defaultTomlConfig
    -- 4. toml explicitly disabled
    TomlNone -> pure defaultTomlConfig

  -- merge shared CLI and toml values
  (xdgState,) <$> Merged.mergeConfigs args tomlConfig
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwM tomlErr

printIndex ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  ListParams ConfigPhaseMerged ->
  m ()
printIndex listCmd = do
  index <- Charon.getIndex
  formatted <- Index.formatIndex listCmd index
  putTextLn formatted

printMetadata ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m ()
printMetadata = Charon.getMetadata >>= prettyDel

prettyDel :: (Display a, MonadTerminal m) => a -> m ()
prettyDel = putTextLn . U.renderPretty

readTrashEntryCompletions ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m [Text]
readTrashEntryCompletions xdgState = do
  exists <- doesFileExist compsPath
  if exists
    then do
      contents <- readFileUtf8ThrowM compsPath
      pure $ T.lines $ T.strip contents
    else pure []
  where
    compsPath = mkTrashCompletionsPath xdgState
{-# INLINEABLE readTrashEntryCompletions #-}

saveTrashEntryCompletions ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixFilesC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  OsPath ->
  Either SomeException () ->
  m ()
saveTrashEntryCompletions xdgState eMainResult = do
  -- Try to save completions, but we do not want an error to throw.
  eSaveResult <- trySync $ do
    index <- Charon.getIndex
    let entryNames = toEntryName <$> index ^. #unIndex
    PW.createDirectoryIfMissing True xdgState
    writeFileUtf8 compsPath (formatNames entryNames)

  -- We use the result of the main program to determine if we should print
  -- an error when saving the completions file fails.
  case (eMainResult, eSaveResult) of
    -- 1. Completions save succeeded: No need to do anything.
    (_, Right _) -> pure ()
    -- 2. Main program succeeded but completions save failed. This is worth
    -- an error message.
    (Right _, Left ex) -> do
      -- If something went wrong, do not take any changes; try to delete the
      -- completions file.
      putTextLn $ "Failed saving completions: " <> displayExceptiont ex
      void $ trySync $ PW.removeFileIfExists_ compsPath
    -- 3. Both main program and completions failed. We do not print anything
    -- in this case, as it is possible (though, admittedly, not guaranteed),
    -- that the same error is involved, hence redundant.
    (Left _, Left _) -> void $ trySync $ PW.removeFileIfExists_ compsPath
  where
    toEntryName =
      packText
        . decodeLenient
        . view (_1 % #fileName % #unPathI)

    formatNames =
      T.intercalate "\n"
        . Set.toList
        . Set.fromList
        . F.toList

    compsPath = mkTrashCompletionsPath xdgState
{-# INLINEABLE saveTrashEntryCompletions #-}

mkTrashCompletionsPath :: OsPath -> OsPath
mkTrashCompletionsPath xdgState = xdgState </> [osp|completions.txt|]
