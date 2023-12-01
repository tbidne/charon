{-# LANGUAGE TemplateHaskell #-}

module Charon.Backend.Default.Index
  ( readIndex,
  )
where

import Charon.Backend.Data qualified as Backend.Data
import Charon.Backend.Default.BackendArgs (BackendArgs)
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Class.Serial (Serial (DecodeExtra))
import Charon.Class.Serial qualified as Serial
import Charon.Data.Index (Index (MkIndex))
import Charon.Data.PathData (PathData)
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryPath,
        TrashHome
      ),
    (<//>),
  )
import Charon.Exception
  ( InfoDecodeE (MkInfoDecodeE),
    TrashEntryFileNotFoundE (MkTrashEntryFileNotFoundE),
    TrashEntryInfoBadExtE (MkTrashEntryInfoBadExtE),
    TrashEntryInfoNotFoundE (MkTrashEntryInfoNotFoundE),
  )
import Charon.Prelude
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import System.OsPath qualified as FP

type Acc = (Seq (PathData, PathI TrashEntryPath), HashSet (PathI TrashEntryFileName))

-- | Reads the trash directory into the 'Index'. If this succeeds then
-- everything is 'well-formed' i.e. there is a bijection between trash/files
-- and trash/info.
readIndex ::
  forall m (pd :: Type) k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadFileReader m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  m Index
readIndex backendArgs trashHome = addNamespace "readIndex" $ do
  paths <- listDirectory trashInfoDir'
  $(logDebug) ("Trash info: " <> decodeOsToFpDisplayExT trashInfoDir')
  $(logDebug) ("Info: " <> T.intercalate ", " (decodeOsToFpDisplayExT <$> paths))

  -- TODO: Maybe this shouldn't report errors eagerly

  let seqify :: OsPath -> m Acc -> m Acc
      seqify p macc = do
        let actualExt = FP.takeExtension p
            expectedExt = Backend.Data.backendExt (backendArgs ^. #backend)
            toCorePathData = backendArgs ^. #toCorePathData

        when (actualExt /= expectedExt)
          $ throwCS
          $ MkTrashEntryInfoBadExtE (MkPathI p) actualExt expectedExt

        let path = trashInfoDir' </> p
        $(logDebug) ("Path: " <> decodeOsToFpDisplayExT path)

        contents <- readBinaryFile path
        let -- NOTE: We want the name without the suffix
            fileName = FP.dropExtension $ FP.takeFileName path
            decoded = Serial.decode @pd (MkPathI fileName) contents
        case decoded of
          Left err -> throwCS $ MkInfoDecodeE (MkPathI path) contents err
          Right pd -> do
            trashEntryPath <- getTrashEntryPath @_ @pd trashHome pd
            (accSeq, accSet) <- macc
            corePathData <- toCorePathData trashHome pd
            pure ((corePathData, trashEntryPath) :<| accSeq, HSet.insert (corePathData ^. #fileName) accSet)

  (indexSeq, pathSet) <- foldr seqify (pure (Seq.empty, HSet.empty)) paths

  -- NOTE: Check that all files in /files exist in the index.
  allTrashPaths <- listDirectory trashPathsDir'
  $(logDebug) ("Paths: " <> T.intercalate ", " (decodeOsToFpDisplayExT <$> allTrashPaths))
  for_ allTrashPaths $ \p -> do
    let pName = MkPathI $ FP.takeFileName p
    unless (pName `HSet.member` pathSet)
      $ throwCS
      $ MkTrashEntryInfoNotFoundE trashHome pName

  pure $ MkIndex indexSeq
  where
    MkPathI trashPathsDir' = Default.Utils.getTrashPathDir trashHome
    MkPathI trashInfoDir' = Default.Utils.getTrashInfoDir trashHome

-- | Like 'throwIfTrashNonExtant' except returns the path if it exists.
getTrashEntryPath ::
  forall m pd k.
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  pd ->
  m (PathI TrashEntryPath)
getTrashEntryPath trashHome pd = addNamespace "getTrashEntryPath" $ do
  lookupTrashPath trashHome pd >>= \case
    Just trashPath -> pure trashPath
    Nothing ->
      throwCS
        $ MkTrashEntryFileNotFoundE trashHome filePath
  where
    filePath = pd ^. #fileName

-- | Like 'trashPathExists' except returns the path if it exists.
lookupTrashPath ::
  forall m pd k.
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  pd ->
  m (Maybe (PathI TrashEntryPath))
lookupTrashPath trashHome pd = do
  -- Unfortunately we don't know the path type, as we could be dealing with
  -- a backend that does not have it on its PathData (e.g. fdo). Thus we
  -- have to check symlinks first, then doesPathExist for files/dirs.
  exists <- doesAnyPathExist trashPath'
  pure
    $ if exists
      then Just trashPath
      else Nothing
  where
    -- NOTE: doesPathExist rather than doesFile/Dir... as that requires knowing
    -- the path type. See Note [PathData PathType conditions].
    trashPath :: PathI 'TrashEntryPath
    trashPath@(MkPathI trashPath') =
      trashHome <//> MkPathI Default.Utils.pathFiles <//> (pd ^. #fileName)
