{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides functionality for moving a file to a trash location.
module SafeRm
  ( -- * Delete
    delete,
    permDelete,
    emptyTrash,

    -- * Restore
    restore,

    -- * Information
    getIndex,
    getMetadata,

    -- * Transformations
    convert,
    merge,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import Effectful.FileSystem.HandleWriter.Dynamic qualified as HW
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Dynamic (getSystemTime)
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Index (Index)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Prelude
import SafeRm.Trash qualified as Trash
import SafeRm.Utils qualified as U
import System.IO qualified as IO

-- NOTE: [Effectful Type Inference]
--
-- Effectful can sometimes choke on type inference (e.g. with Reader), so we
-- need to manually add type annotations. This is annoying, so there is a
-- GHC plugin provided in effectful-plugin. Unfortunately, this does not seem
-- to work with nix i.e. 'nix build' was throwing an error about being unable
-- to find the required plugin, despite it existing. This is especially weird
-- as the package built and all tests passes. It was only at the very end
-- when GHC complained.
--
-- Thus, we manually provide annotations instead of using the plugin.
-- Hopefully one day the type inference will be fixed w/o the need for the
-- plugin.

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/safe-rm/@.
delete ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    FileWriterDynamic :> es,
    IORefStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es,
    TimeDynamic :> es
  ) =>
  UniqueSeq (PathI TrashEntryOriginalPath) ->
  Eff es ()
delete paths = addNamespace "delete" $ do
  trashHome <- asks @env getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  Trash.createTrash @env

  someExRef <- newIORef Nothing
  currTime <- MkTimestamp <$> getSystemTime

  -- move paths to trash
  addNamespace "deleting" $ for_ paths $ \fp ->
    Trash.mvOriginalToTrash @env trashHome currTime fp
      `catchAny` \ex -> do
        $(logWarn) (T.pack $ displayException ex)
        putStrLn
          $ mconcat
            [ "Error deleting path '",
              decodeOsToFpShow $ fp ^. #unPathI,
              "': ",
              displayException ex
            ]
        writeIORef someExRef (Just ex)

  U.throwIfEx someExRef

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    Concurrent :> es,
    FileReaderDynamic :> es,
    HandleWriterDynamic :> es,
    IORefStatic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    PosixCompatStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  Eff es ()
permDelete force paths = addNamespace "permDelete" $ do
  trashHome <- asks @env getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  someExRef <- newIORef Nothing

  -- permanently delete paths
  addNamespace "deleting" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.permDeleteFromTrash @env force trashHome p >>= U.setRefIfJust someExRef)
      `catchAny` \ex -> do
        $(logWarn) (T.pack $ displayException ex)
        putStrLn
          $ mconcat
            [ "Error permanently deleting path '",
              decodeOsToFpShow $ p ^. #unPathI,
              "': ",
              displayException ex
            ]
        -- in case Trash.permDeleteFromTrash throws an exception
        writeIORef someExRef (Just ex)

  U.throwIfEx someExRef

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env es.
  ( HasBackend env,
    HasTrashHome env,
    FileReaderDynamic :> es,
    PathReaderDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    Reader env :> es
  ) =>
  Eff es Index
getIndex = addNamespace "getIndex" $ do
  trashHome <- asks @env getTrashHome

  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  Trash.doesTrashExist @env >>= \case
    True -> Index.readIndex @env trashHome
    False -> do
      $(logDebug) "Trash does not exist."
      pure Index.empty

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall env es.
  ( HasBackend env,
    HasTrashHome env,
    FileReaderDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    Reader env :> es
  ) =>
  Eff es Metadata
getMetadata = addNamespace "getMetadata" $ do
  trashHome <- asks @env getTrashHome
  trashLog <- Env.getTrashLog
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)
  Trash.doesTrashExist @env >>= \case
    True -> Metadata.toMetadata @env (trashHome, trashLog)
    False -> do
      $(logInfo) "Trash does not exist."
      pure Metadata.empty

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    FileReaderDynamic :> es,
    IORefStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  Eff es ()
restore paths = addNamespace "restore" $ do
  trashHome <- asks @env getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  someExRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.restoreTrashToOriginal @env trashHome p >>= U.setRefIfJust someExRef)
      `catchAny` \ex -> do
        $(logWarn) (T.pack $ displayException ex)
        putStrLn
          $ mconcat
            [ "Error restoring path '",
              decodeOsToFpShow $ p ^. #unPathI,
              "': ",
              displayException ex
            ]
        writeIORef someExRef (Just ex)

  U.throwIfEx someExRef

-- | Empties the trash.
emptyTrash ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    FileReaderDynamic :> es,
    HandleWriterDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Bool ->
  Eff es ()
emptyTrash force = addNamespace "emptyTrash" $ do
  trashHome@(MkPathI th) <- asks @env getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  exists <- doesDirectoryExist th
  if not exists
    then do
      $(logDebug) "Trash home does not exist."
      putTextLn $ Paths.toText trashHome <> " is empty."
    else
      if force
        then do
          removeDirectoryRecursive th
          Trash.createTrash @env
        else do
          noBuffering
          metadata <- getMetadata @env
          putStrLn ""
          putTextLn $ U.renderPretty metadata
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> Term.getChar
          if
            | c == 'y' -> do
                $(logDebug) "Deleting contents."
                removeDirectoryRecursive th
                Trash.createTrash @env
                putStrLn ""
            | c == 'n' -> do
                $(logDebug) "Not deleting contents."
                putStrLn ""
            | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

convert ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    FileReaderDynamic :> es,
    FileWriterDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Backend ->
  Eff es ()
convert dest = addNamespace "convert" $ do
  trashHome <- asks @env getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  src <- asks @env getBackend
  if src == dest
    then do
      let msg =
            mconcat
              [ "--backend == requested conversion type: " <> Backend.backendArg dest,
                ". Nothing to do."
              ]
      $(logDebug) $ T.pack msg
      putStrLn msg
    else do
      $(logDebug) $ "Current backend: " <> T.pack (Backend.backendArg src)
      Trash.convertBackend @env dest

merge ::
  forall env es.
  ( HasTrashHome env,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  PathI TrashHome ->
  Eff es ()
merge dest = addNamespace "merge" $ do
  src <- asks @env getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText src)

  src' <- Paths.liftPathIF' canonicalizePath src
  dest' <- Paths.liftPathIF' canonicalizePath dest

  if src' == dest'
    then do
      let msg =
            mconcat
              [ "Source path ",
                decodeOsToFpShow $ src' ^. #unPathI,
                " is the same as dest path ",
                decodeOsToFpShow $ dest' ^. #unPathI,
                ". Nothing to do."
              ]
      $(logDebug) $ T.pack msg
      putStrLn msg
    else do
      $(logDebug) $ "Dest path: " <> Paths.toText dest
      Trash.mergeTrashDirs src dest

noBuffering :: (HandleWriterDynamic :> es) => Eff es ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = HW.hSetBuffering h NoBuffering
