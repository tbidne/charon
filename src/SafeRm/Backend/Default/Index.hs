{-# LANGUAGE TemplateHaskell #-}

module SafeRm.Backend.Default.Index
  ( readIndex,
  )
where

import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import SafeRm.Backend.Data qualified as Backend.Data
import SafeRm.Backend.Default.BackendArgs (BackendArgs)
import SafeRm.Backend.Default.Utils qualified as Default.Utils
import SafeRm.Data.Index (Index (MkIndex))
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryPath,
        TrashHome
      ),
    (<//>),
  )
import SafeRm.Data.Serialize (Serialize (DecodeExtra))
import SafeRm.Data.Serialize qualified as Serialize
import SafeRm.Exception
  ( InfoDecodeE (MkInfoDecodeE),
    TrashEntryFileNotFoundE (MkTrashEntryFileNotFoundE),
    TrashEntryInfoBadExtE (MkTrashEntryInfoBadExtE),
    TrashEntryInfoNotFoundE (MkTrashEntryInfoNotFoundE),
  )
import SafeRm.Prelude
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  m Index
readIndex backendArgs trashHome = addNamespace "readIndex" $ do
  paths <- listDirectory trashInfoDir'
  $(logDebug) ("Trash info: " <> decodeOsToFpShowText trashInfoDir')
  $(logDebug) ("Info: " <> T.intercalate ", " (decodeOsToFpShowText <$> paths))

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
        $(logDebug) ("Path: " <> decodeOsToFpShowText path)

        contents <- readBinaryFile path
        let -- NOTE: We want the name without the suffix
            fileName = FP.dropExtension $ FP.takeFileName path
            decoded = Serialize.decode @pd (MkPathI fileName) contents
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
  $(logDebug) ("Paths: " <> T.intercalate ", " (decodeOsToFpShowText <$> allTrashPaths))
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
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  pd ->
  m (PathI TrashEntryPath)
getTrashEntryPath trashHome pd = do
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
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  pd ->
  m (Maybe (PathI TrashEntryPath))
lookupTrashPath trashHome pd = do
  exists <- doesPathExist trashPath'
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
