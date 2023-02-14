{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides functionality for moving a file to a trash location.
--
-- @since 0.1
module SafeRm
  ( -- * Delete
    delete,
    deletePermanently,
    emptyTrash,

    -- * Restore
    restore,

    -- * Information
    getIndex,
    getMetadata,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.System.Terminal
  ( MonadTerminal (getChar),
  )
import Effects.Time (getSystemTime)
import SafeRm.Data.Index (Index)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashName),
  )
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Prelude
import SafeRm.Trash qualified as Trash
import System.IO qualified as IO

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to @~\/.trash@.
--
-- @since 0.1
delete ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeq (PathI OriginalPath) ->
  m ()
delete paths = addNamespace "delete" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  Trash.createTrash

  anyFailedRef <- newIORef False
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
        writeIORef anyFailedRef True

  anyFailed <- readIORef anyFailedRef
  when anyFailed exitFailure

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
deletePermanently ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNamespace m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Bool ->
  UniqueSeq (PathI TrashName) ->
  m ()
deletePermanently force paths = addNamespace "deletePermanently" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  anyFailedRef <- newIORef False

  -- permanently delete paths
  addNamespace "deleting" $ for_ paths $ \p ->
    Trash.deleteTrashPath force trashHome p
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error permanently deleting path '",
              p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef anyFailedRef True

  anyFailed <- readIORef anyFailedRef
  when anyFailed exitFailure

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
--
-- @since 0.1
getIndex ::
  forall env m.
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    HasTrashHome env,
    MonadLoggerNamespace m,
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
--
-- @since 0.1
getMetadata ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  trashHome <- asks getTrashHome
  let trashLog = Env.getTrashLog trashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))
  Trash.doesTrashExist >>= \case
    True -> Metadata.toMetadata (trashHome, trashLog)
    False -> do
      $(logInfo) "Trash does not exist."
      pure mempty

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location. If @trash@ is not given then we look in the default location
-- e.g. @~\/.trash@.
--
-- @since 0.1
restore ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  UniqueSeq (PathI TrashName) ->
  m ()
restore paths = addNamespace "restore" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  anyFailedRef <- newIORef False

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ paths $ \p ->
    Trash.mvTrashToOriginal trashHome p
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error restoring path '",
              p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef anyFailedRef True

  anyFailed <- readIORef anyFailedRef
  when anyFailed exitFailure

-- | Empties the trash.
--
-- @since 0.1
emptyTrash ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNamespace m,
    MonadReader env m,
    MonadTerminal m
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

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = hSetBuffering h NoBuffering
