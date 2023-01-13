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
import Effects.FileSystem.MonadHandleWriter (MonadHandleWriter (..))
import Effects.MonadTerminal
  ( MonadTerminal (getChar),
    putTextLn,
  )
import Effects.MonadTime (getSystemTime)
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
import SafeRm.Exception
  ( Exceptions (MkExceptions),
    PathNotFoundE (MkPathNotFoundE),
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
    MonadCallStack m,
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

  deletedPathsRef <- newIORef HMap.empty
  exceptionsRef <- newIORef Nothing
  currTime <- MkTimestamp <$> getSystemTime

  -- move path to trash, saving any exceptions
  addNamespace "deleting" $ for_ paths $ \fp ->
    ( do
        pd <- PathData.toPathData currTime trashHome fp
        $(logDebug) (showt pd)
        PathData.mvOriginalToTrash trashHome pd
        modifyIORef' deletedPathsRef (HMap.insert (pd ^. #fileName) pd)
    )
      `catchAny` \ex -> do
        $(logWarn) (T.pack $ displayCallStack ex)
        modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

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

  $(logInfo) ("Deleted: " <> showMapOrigPaths deletedPaths)

  exceptions <- readIORef exceptionsRef
  Utils.whenJust exceptions $
    -- NOTE: We do not log these exceptions. In general, the only exceptions
    -- we log in the SafeRm API are those that are non-fatal i.e. ones we
    -- catch. Unhandled ones are left to bubble up, where the runner can
    -- catch and log them.
    --
    -- Also notice that we use throwIO here and not throwWithCallStack. The
    -- Exceptions type is uses solely to aggregate exceptions encountered here,
    -- deletePermanently, and restore. All the exceptions it wraps should have
    -- their own call stack data, so adding more clutters the output for little
    -- gain.
    throwIO . MkExceptions

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
deletePermanently ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadCallStack m,
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
  exceptionsRef <- newIORef Nothing

  let deleteFn pd =
        ( do
            $(logDebug) (showt pd)
            PathData.deletePathData trashHome pd
            modifyIORef' deletedPathsRef (HMap.insert (pd ^. #fileName) pd)
        )
          `catchAny` \ex -> do
            $(logWarn) (T.pack $ displayCallStack ex)
            modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

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

  $(logInfo) ("Deleted: " <> showMapTrashPaths deletedPaths)

  exceptions <- readIORef exceptionsRef
  let searchExs = pathsToException searchFailures
  Utils.whenJust (Utils.concatMNonEmpty searchExs exceptions) $
    throwIO . MkExceptions

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
--
-- @since 0.1
getIndex ::
  forall env m.
  ( HasCallStack,
    MonadCallStack m,
    MonadFileReader m,
    MonadPathReader m,
    HasTrashHome env,
    MonadLoggerNamespace m,
    MonadReader env m
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
    MonadCallStack m,
    MonadFileReader m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  paths@(trashHome, indexPath, _) <- asks getTrashPaths
  $(logDebug) ("Trash home: " <> T.pack (trashHome ^. #unPathI))
  Paths.applyPathI doesFileExist indexPath >>= \case
    True -> Metadata.toMetadata paths
    False -> do
      $(logDebug) "Index does not exist."
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
    MonadLoggerNamespace m,
    MonadCallStack m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m
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
  exceptionsRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ toRestore $ \pd ->
    ( do
        $(logDebug) (showt pd)
        PathData.mvTrashToOriginal trashHome pd
        modifyIORef' restoredPathsRef (HMap.insert (pd ^. #fileName) pd)
    )
      `catchAny` \ex -> do
        $(logWarn) (T.pack $ displayCallStack ex)
        modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

  -- override old index
  restoredPaths <- readIORef restoredPathsRef
  Index.writeIndex indexPath (MkIndex $ HMap.difference indexMap restoredPaths)

  $(logInfo) ("Restored: " <> showMapOrigPaths restoredPaths)

  exceptions <- readIORef exceptionsRef
  let searchExs = pathsToException searchFailures
  Utils.whenJust (Utils.concatMNonEmpty searchExs exceptions) $
    throwIO . MkExceptions

-- | Empties the trash. Deletes the index file.
--
-- @since 0.1
emptyTrash ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadCallStack m,
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

noBuffering :: (HasCallStack, MonadCallStack m, MonadHandleWriter m) => m ()
noBuffering = addCallStack $ buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = hSetBuffering h NoBuffering

pathsToException ::
  Foldable t =>
  t (PathI 'TrashName) ->
  [SomeException]
pathsToException = foldr go []
  where
    go fp acc = pathToException fp : acc

pathToException :: PathI TrashName -> SomeException
pathToException (MkPathI p) = toException $ MkPathNotFoundE p
