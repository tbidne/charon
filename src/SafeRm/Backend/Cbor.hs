{-# LANGUAGE TemplateHaskell #-}

-- | The Cbor backend.
module SafeRm.Backend.Cbor
  ( -- * Delete
    delete,
    permDelete,
    emptyTrash,

    -- * Restore
    restore,

    -- * Information
    lookupTrashName,
    getIndex,
    getMetadata,

    -- * Transformations
    convert,
    merge,
  )
where

import Data.Char qualified as Ch
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter qualified as HW
import Effects.System.Terminal qualified as Term
import Effects.Time (getSystemTime)
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Index (Index)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Exception (EmptySearchResults (MkEmptySearchResults))
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
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  Trash.createTrash

  someExRef <- newIORef Nothing
  currTime <- MkTimestamp <$> getSystemTime

  -- move paths to trash
  addNamespace "deleting" $ for_ paths $ \fp ->
    Trash.mvOriginalToTrash trashHome currTime fp
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn
          $ mconcat
            [ "Error deleting path '",
              decodeOsToFpShow $ fp ^. #unPathI,
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
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
permDelete force paths = addNamespace "permDelete" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  someExRef <- newIORef Nothing

  -- permanently delete paths
  addNamespace "deleting" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.permDeleteFromTrash force trashHome p >>= U.setRefIfJust someExRef)
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn
          $ mconcat
            [ "Error permanently deleting path '",
              decodeOsToFpShow $ p ^. #unPathI,
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

  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  Trash.doesTrashExist >>= \case
    True -> Index.readIndex trashHome
    False -> do
      $(logDebug) "Trash does not exist."
      pure Index.empty

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  trashHome <- asks getTrashHome
  trashLog <- Env.getTrashLog
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)
  Trash.doesTrashExist >>= \case
    True -> Metadata.toMetadata (trashHome, trashLog)
    False -> do
      $(logInfo) "Trash does not exist."
      pure Metadata.empty

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
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  someExRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.restoreTrashToOriginal trashHome p >>= U.setRefIfJust someExRef)
      `catchAnyCS` \ex -> do
        $(logWarn) (T.pack $ displayNoCS ex)
        putStrLn
          $ mconcat
            [ "Error restoring path '",
              decodeOsToFpShow $ p ^. #unPathI,
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
          Trash.createTrash
        else do
          noBuffering
          metadata <- getMetadata
          putStrLn ""
          putTextLn $ U.renderPretty metadata
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> Term.getChar
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
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

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

-- | Looks up the trash entry file name, throwing an exception if none is
-- found.
lookupTrashName ::
  ( HasBackend env,
    HasTrashHome env,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m (NESeq PathData)
lookupTrashName pathNames = addNamespace "lookupTrashName" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  results <- traverse (Trash.findPathData trashHome) (pathNames ^. #seq)
  case results of
    -- TODO: This __should__ be impossible as the UniqueSeq is non-empty, though
    -- it would be nice to have a better type for this (i.e. UniqueSeqNE).
    Seq.Empty -> throwCS $ MkEmptySearchResults pathNames
    (x :<|| xs) :<| ys -> pure $ x :<|| (xs <> (ys >>= NESeq.toSeq))

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = HW.hSetBuffering h NoBuffering
