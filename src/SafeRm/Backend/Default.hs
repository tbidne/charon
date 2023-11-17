{-# LANGUAGE TemplateHaskell #-}

-- | This module (and sub-hierarchy) provides logic that can be used by any
-- backend (i.e. cbor, fdo, json) that has the following structure:
--
-- @
--   trash/paths/ (paths)
--   trash/info/  (info files)
-- @
--
-- The only difference is in the (de)serialization.
module SafeRm.Backend.Default
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
    merge,
  )
where

import Data.Bytes qualified as Bytes
import Data.Char qualified as Ch
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.System.Terminal qualified as Term
import Effects.Time (getSystemTime)
import GHC.Real (Integral)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Literal.Rational (FromRational (afromRational))
import SafeRm.Backend.Default.BackendArgs (BackendArgs)
import SafeRm.Backend.Default.Index qualified as Default.Index
import SafeRm.Backend.Default.Trash qualified as Trash
import SafeRm.Backend.Default.Utils qualified as Default.Utils
import SafeRm.Data.Index (Index)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata (MkMetadata))
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.PathData qualified as PathData.Core
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashHome
      ),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize (Serialize (DecodeExtra))
import SafeRm.Data.Timestamp (Timestamp (MkTimestamp))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend, HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Exception (EmptySearchResults (MkEmptySearchResults))
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/safe-rm/@.
delete ::
  forall m env pd k.
  ( HasBackend env,
    HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m,
    Serialize pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  UniqueSeq (PathI TrashEntryOriginalPath) ->
  m ()
delete backendArgs paths = addNamespace "delete" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  void Trash.createTrash

  someExRef <- newIORef Nothing
  currTime <- MkTimestamp <$> getSystemTime

  -- move paths to trash
  addNamespace "deleting" $ for_ paths $ \fp ->
    Trash.mvOriginalToTrash backendArgs trashHome currTime fp
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

  Utils.throwIfEx someExRef

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
permDelete backendArgs force paths = addNamespace "permDelete" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  someExRef <- newIORef Nothing

  -- permanently delete paths
  addNamespace "deleting" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.permDeleteFromTrash backendArgs force trashHome p >>= Utils.setRefIfJust someExRef)
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

  Utils.throwIfEx someExRef

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadThrow m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  m Index
getIndex backendArgs = addNamespace "getIndex" $ do
  trashHome <- asks getTrashHome

  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  Trash.doesTrashExist >>= \case
    True -> Default.Index.readIndex backendArgs trashHome
    False -> do
      $(logDebug) "Trash does not exist."
      pure Index.empty

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  m Metadata
getMetadata backendArgs = addNamespace "getMetadata" $ do
  trashHome <- asks getTrashHome
  trashLog <- Env.getTrashLog
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  let MkPathI trashPathsDir = Default.Utils.getTrashPathDir trashHome

  Trash.doesTrashExist >>= \case
    False -> do
      $(logInfo) "Trash does not exist."
      pure Metadata.empty
    True -> do
      -- Index size
      index <- view #unIndex <$> Default.Index.readIndex backendArgs trashHome
      let numIndex = length index
      $(logDebug) ("Index size: " <> showt numIndex)

      -- Num entries
      numEntries <- foldl' (\acc _ -> acc + 1) 0 <$> listDirectory trashPathsDir
      $(logDebug) ("Num entries: " <> showt numEntries)

      -- Log size
      let logPath = trashLog ^. #unPathI
      logExists <- doesFileExist logPath
      logSize <-
        if logExists
          then Bytes.normalize . toDouble . MkBytes @B <$> getFileSize logPath
          else do
            $(logDebug) "Log does not exist"
            pure (afromRational 0)

      -- Summed size
      allFiles <- Utils.getAllFiles trashPathsDir
      let allSizes = foldl' (\acc (pd, _) -> (pd ^. #size) .+. acc) zero index
          numFiles = length allFiles
          size = fromIntegral <$> Bytes.normalize allSizes

      $(logDebug) ("Num all files: " <> showt numFiles)
      $(logDebug) ("Total size: " <> showt size)

      -- NOTE: If the index is successfully read then we have verified that
      -- all invariants are preserved i.e. bijection between /files and /info.

      pure
        $ MkMetadata
          { numEntries = toNat numEntries,
            numFiles = toNat numFiles,
            logSize,
            size
          }
  where
    toDouble :: (Integral a) => Bytes s a -> Bytes s Double
    toDouble = fmap fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
restore backendArgs paths = addNamespace "restore" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  someExRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.restoreTrashToOriginal backendArgs trashHome p >>= Utils.setRefIfJust someExRef)
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

  Utils.throwIfEx someExRef

-- | Empties the trash.
emptyTrash ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  Bool ->
  m ()
emptyTrash backendArgs force = addNamespace "emptyTrash" $ do
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
          void Trash.createTrash
        else do
          Utils.noBuffering
          metadata <- getMetadata backendArgs
          putStrLn ""
          putTextLn $ Utils.renderPretty metadata
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> Term.getChar
          if
            | c == 'y' -> do
                $(logDebug) "Deleting contents."
                removeDirectoryRecursive th
                void Trash.createTrash
                putStrLn ""
            | c == 'n' -> do
                $(logDebug) "Not deleting contents."
                putStrLn ""
            | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

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

type PathDataCore = PathData.Core.PathData

-- | Looks up the trash entry file name, throwing an exception if none is
-- found.
lookupTrashName ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadLoggerNS m,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m (NESeq PathDataCore)
lookupTrashName backendArgs pathNames = addNamespace "lookupTrashName" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  results <- traverse (Trash.findPathData backendArgs trashHome) (pathNames ^. #seq)
  case results of
    -- TODO: This __should__ be impossible as the UniqueSeq is non-empty, though
    -- it would be nice to have a better type for this (i.e. UniqueSeqNE).
    Seq.Empty -> throwCS $ MkEmptySearchResults pathNames
    (x :<|| xs) :<| ys -> pure $ x :<|| (xs <> (ys >>= NESeq.toSeq))
