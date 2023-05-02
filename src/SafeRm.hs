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
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.System.Terminal
  ( MonadTerminal (getChar),
  )
import Effects.Time (getSystemTime)
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Index (Index)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (..),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend (..), HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Prelude
import SafeRm.Trash qualified as Trash
import SafeRm.Utils qualified as U
import System.IO qualified as IO

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/safe-rm/@.
delete ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeq (PathI TrashEntryOriginalPath) ->
  m ()
delete paths = addNamespace "delete" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  Trash.createTrash

  someExRef <- newIORef Nothing
  currTime <- MkTimestamp <$> getSystemTime

  -- move paths to trash
  addNamespace "deleting" $ for_ paths $ \fp ->
    Trash.mvOriginalToTrash trashHome currTime fp
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error deleting path '",
              fp ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef someExRef (Just ex)

  U.throwIfEx someExRef

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
permDelete force paths = addNamespace "permDelete" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  someExRef <- newIORef Nothing

  -- permanently delete paths
  addNamespace "deleting" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.permDeleteFromTrash force trashHome p >>= U.setRefIfJust someExRef)
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error permanently deleting path '",
              p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        -- in case Trash.permDeleteFromTrash throws an exception
        writeIORef someExRef (Just ex)

  U.throwIfEx someExRef

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Index
getIndex = addNamespace "getIndex" $ do
  trashHome <- asks getTrashHome

  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  Trash.doesTrashExist >>= \case
    True -> Index.readIndex trashHome
    False -> do
      $(logDebug) "Trash does not exist."
      pure mempty

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  trashHome <- asks getTrashHome
  trashLog <- Env.getTrashLog
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))
  Trash.doesTrashExist >>= \case
    True -> Metadata.toMetadata (trashHome, trashLog)
    False -> do
      $(logInfo) "Trash does not exist."
      pure mempty

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
restore paths = addNamespace "restore" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  someExRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.restoreTrashToOriginal trashHome p >>= U.setRefIfJust someExRef)
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error restoring path '",
              p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef someExRef (Just ex)

  U.throwIfEx someExRef

-- | Empties the trash.
emptyTrash ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Bool ->
  m ()
emptyTrash force = addNamespace "emptyTrash" $ do
  trashHome@(MkPathI th) <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  exists <- doesDirectoryExist th
  if not exists
    then do
      $(logDebug) "Trash home does not exist."
      putTextLn $ T.pack th <> " is empty."
    else
      if force
        then do
          removeDirectoryRecursive th
          Trash.createTrash
        else do
          noBuffering
          metadata <- getMetadata
          putStrLn ""
          putTextLn $ U.renderPretty metadata
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> getChar
          if
              | c == 'y' -> do
                  $(logDebug) "Deleting contents."
                  removeDirectoryRecursive th
                  Trash.createTrash
                  putStrLn ""
              | c == 'n' -> do
                  $(logDebug) "Not deleting contents."
                  putStrLn ""
              | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

convert ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Backend ->
  m ()
convert dest = addNamespace "convert" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  src <- asks getBackend
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
      Trash.convertBackend dest

merge ::
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  m ()
merge dest = addNamespace "merge" $ do
  src <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (src ^. #unPathI))

  src' <- Paths.liftPathIF' canonicalizePath src
  dest' <- Paths.liftPathIF' canonicalizePath dest

  if src' == dest'
    then do
      let msg =
            mconcat
              [ "Source path '",
                src' ^. #unPathI,
                "' is the same as dest path '",
                dest' ^. #unPathI,
                "'. Nothing to do."
              ]
      $(logDebug) $ T.pack msg
      putStrLn msg
    else do
      $(logDebug) $ "Dest path: " <> T.pack (dest' ^. #unPathI)
      Trash.mergeTrashDirs src dest

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = hSetBuffering h NoBuffering
