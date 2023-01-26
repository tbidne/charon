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
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.System.Terminal
  ( MonadTerminal (getChar),
  )
import Effects.Time (getSystemTime)
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.Index (Index (MkIndex))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashName),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env
  ( HasTrashHome (getTrashHome),
    getTrashIndex,
    getTrashPaths,
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils
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
  (trashHome, indexPath, _) <- asks getTrashPaths
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  Paths.applyPathI (createDirectoryIfMissing False) trashHome

  anyFailedRef <- newIORef False
  deletedPathsRef <- newIORef HMap.empty
  currTime <- MkTimestamp <$> getSystemTime

  -- move path to trash, saving any exceptions
  addNamespace "deleting" $ for_ paths $ \fp ->
    ( do
        pd <- PathData.toPathData currTime trashHome fp
        $(logDebug) (showt pd)
        PathData.mvOriginalToTrash trashHome pd
        modifyIORef' deletedPathsRef (HMap.insert (pd ^. #fileName) pd)
        $(logDebug) ("Deleted: " <> showt pd)
    )
      `catchAnyWithCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error deleting path '",
              fp ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef anyFailedRef True

  -- override old index
  deletedPaths <- readIORef deletedPathsRef
  nonEmpty <-
    Utils.allM1
      [ Paths.applyPathI doesFileExist indexPath,
        Paths.applyPathI (fmap (> afromInteger 0) . getFileSize) indexPath
      ]
  if nonEmpty
    then Index.appendIndex indexPath (MkIndex deletedPaths)
    else Index.writeIndex indexPath (MkIndex deletedPaths)

  $(logInfo) ("Written to index: " <> showMapOrigPaths deletedPaths)

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
    MonadFileWriter m,
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
  (trashHome, indexPath, _) <- asks getTrashPaths
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))

  index <- Index.readIndex indexPath
  let indexMap = index ^. #unIndex
      (searchFailures, toDelete) = Index.searchIndex paths index

  deletedPathsRef <- newIORef HMap.empty
  anyFailedRef <- newIORef False

  -- report errors
  unless (null searchFailures) $ do
    let failureStr = Paths.showPaths searchFailures
    $(logWarn) (T.pack failureStr)
    putStrLn $ "Could not find paths in trash: " <> failureStr
    writeIORef anyFailedRef True

  let deleteFn pd =
        ( do
            $(logDebug) ("Deleting: " <> showt pd)
            PathData.deletePathData trashHome pd
            modifyIORef' deletedPathsRef (HMap.insert (pd ^. #fileName) pd)
            $(logDebug) ("Permanently deleted: " <> showt pd)
        )
          `catchAnyWithCS` \ex -> do
            $(logWarn) (T.pack $ displayNoCS ex)
            putStrLn $
              mconcat
                [ "Error permanently deleting path '",
                  show pd,
                  "': ",
                  displayNoCS ex
                ]
            writeIORef anyFailedRef True

  -- permanently delete paths
  addNamespace "deleting" $
    if force
      then for_ toDelete deleteFn
      else do
        -- NOTE:
        -- - No buffering on input so we can read a single char w/o requiring a
        --   newline to end the input (which then gets passed to getChar, which
        --   interferes with subsequent calls).
        --
        -- - No buffering on output so the "Permanently delete..." string gets
        --   printed w/o the newline.
        noBuffering

        for_ toDelete $ \pd -> do
          let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pd
          putTextLn pdStr
          putStr "Permanently delete (y/n)? "
          c <- Ch.toLower <$> getChar
          if
              | c == 'y' -> deleteFn pd *> putStrLn ""
              | c == 'n' -> putStrLn ""
              | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

  -- override old index
  deletedPaths <- readIORef deletedPathsRef
  Index.writeIndex indexPath (MkIndex $ HMap.difference indexMap deletedPaths)

  $(logInfo) ("Deleted from index: " <> showMapTrashPaths deletedPaths)

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
  indexPath <- asks getTrashIndex
  $(logDebug) ("Index path: " <> T.pack (indexPath ^. #unPathI))
  Paths.applyPathI doesFileExist indexPath >>= \case
    True -> Index.readIndex indexPath
    False -> do
      $(logDebug) "Index does not exist."
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
  paths@(trashHome, indexPath, _) <- asks getTrashPaths
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))
  Paths.applyPathI doesFileExist indexPath >>= \case
    True -> Metadata.toMetadata paths
    False -> do
      $(logInfo) "Index does not exist."
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
    MonadFileWriter m,
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
  (trashHome, indexPath, _) <- asks getTrashPaths
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))
  index <- Index.readIndex indexPath
  let indexMap = index ^. #unIndex
      (searchFailures, toRestore) = Index.searchIndex paths index

  restoredPathsRef <- newIORef HMap.empty
  anyFailedRef <- newIORef False

  -- report errors
  unless (null searchFailures) $ do
    let failureStr = Paths.showPaths searchFailures
    $(logWarn) (T.pack failureStr)
    putStrLn $ "Could not find paths in trash: " <> failureStr
    writeIORef anyFailedRef True

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ toRestore $ \pd ->
    ( do
        $(logDebug) ("Restoring: " <> showt pd)
        PathData.mvTrashToOriginal trashHome pd
        modifyIORef' restoredPathsRef (HMap.insert (pd ^. #fileName) pd)
        $(logDebug) ("Restored: " <> showt pd)
    )
      `catchAnyWithCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn $
          mconcat
            [ "Error restoring path '",
              show pd,
              "': ",
              displayNoCS ex
            ]
        writeIORef anyFailedRef True

  -- override old index
  restoredPaths <- readIORef restoredPathsRef
  Index.writeIndex indexPath (MkIndex $ HMap.difference indexMap restoredPaths)

  $(logInfo) ("Restored from index: " <> showMapOrigPaths restoredPaths)

  anyFailed <- readIORef anyFailedRef
  when anyFailed exitFailure

-- | Empties the trash. Deletes the index file.
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
        then removeDirectoryRecursive th
        else do
          noBuffering
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> getChar
          if
              | c == 'y' -> do
                  $(logDebug) "Deleting contents."
                  removeDirectoryRecursive th
                  putStrLn ""
              | c == 'n' -> do
                  $(logDebug) "Not deleting contents."
                  putStrLn ""
              | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

showMapTrashPaths :: HashMap (PathI 'TrashName) PathData -> Text
showMapTrashPaths = showMapElems #fileName

showMapOrigPaths :: HashMap (PathI 'TrashName) PathData -> Text
showMapOrigPaths = showMapElems #originalPath

showMapElems :: Lens' PathData (PathI i) -> HashMap (PathI TrashName) PathData -> Text
showMapElems toPathI =
  T.intercalate ", "
    . fmap (T.pack . view (toPathI % #unPathI))
    . HMap.elems

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = hSetBuffering h NoBuffering
