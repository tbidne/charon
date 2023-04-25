{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface for backend-agnostic 'PathData' operations. Modules should
-- generally use this instead of the backend-specific modules.
--
-- @since 0.1
module SafeRm.Data.PathData
  ( -- * PathData
    PathData (..),
    PathDataBackend (..),

    -- * Creation
    toPathData,

    -- * Elimination
    deleteFileName,

    -- * Existence
    trashPathExists,
    originalPathExists,

    -- * Operations
    moveFn,

    -- * Miscellaneous
    headerNames,
    normalizeDefault,
    pathDataToTrashPath,
    pathDataToTrashInfoPath,
  )
where

import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathData.Default qualified as Default
import SafeRm.Data.PathData.Fdo qualified as Fdo
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.PathType qualified as PathType
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env qualified as Env
import SafeRm.Prelude

-- | Holds information about a file path.
data PathData
  = PathDataDefault Default.PathData
  | PathDataFdo Fdo.PathData
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty PathData where
  pretty (PathDataDefault pd) = pretty pd
  pretty (PathDataFdo pd) = pretty pd

instance Serialize PathData where
  type DecodeExtra PathData = (PathDataBackend, PathI TrashEntryFileName)
  encode (PathDataDefault pd) = encode pd
  encode (PathDataFdo pd) = encode pd

  decode (PathDataBackendDefault, fileName) bs = PathDataDefault <$> decode fileName bs
  decode (PathDataBackendFdo, fileName) bs = PathDataFdo <$> decode fileName bs

instance
  (k ~ A_Getter, a ~ PathI TrashEntryFileName, b ~ PathI TrashEntryFileName) =>
  LabelOptic "fileName" k PathData PathData a b
  where
  labelOptic = to getter
    where
      getter (PathDataDefault pd) = pd ^. #fileName
      getter (PathDataFdo pd) = pd ^. #fileName
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Getter, a ~ PathI TrashEntryOriginalPath, b ~ PathI TrashEntryOriginalPath) =>
  LabelOptic "originalPath" k PathData PathData a b
  where
  labelOptic = to getter
    where
      getter (PathDataDefault pd) = pd ^. #originalPath
      getter (PathDataFdo pd) = pd ^. #originalPath
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Getter, a ~ Timestamp, b ~ Timestamp) =>
  LabelOptic "created" k PathData PathData a b
  where
  labelOptic = to getter
    where
      getter (PathDataDefault pd) = pd ^. #created
      getter (PathDataFdo pd) = pd ^. #created
  {-# INLINE labelOptic #-}

-- | Type of PathData.
data PathDataBackend
  = -- | PathData for use with the default backend.
    PathDataBackendDefault
  | -- | PathData for use with the FreeDesktopOrg backend.
    PathDataBackendFdo
  deriving stock (Eq, Show)

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- @since 0.1
toPathData ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  PathDataBackend ->
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m PathData
toPathData PathDataBackendDefault ts th = fmap PathDataDefault . Default.toPathData ts th
toPathData PathDataBackendFdo ts th = fmap PathDataFdo . Fdo.toPathData ts th

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists th pd = do
  pathType <- case pd of
    PathDataDefault pd' -> pure (pd' ^. #pathType)
    PathDataFdo pd' -> Fdo.pathDataToType pd'

  PathType.existsFn pathType trashPath'
  where
    MkPathI trashPath' = Env.getTrashPath th (pd ^. #fileName)

-- | Returns 'True' if the 'PathData'\'s @originalPath@ corresponds to a real
-- path that exists.
--
-- @since 0.1
originalPathExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathData ->
  m Bool
originalPathExists pd = do
  pathType <- case pd of
    PathDataDefault pd' -> pure (pd' ^. #pathType)
    PathDataFdo pd' -> Fdo.pathDataToType pd'

  PathType.existsFn pathType (pd ^. (#originalPath % #unPathI))

-- | Deletes the pathdata's fileName from the trashHome.
deleteFileName ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m ()
deleteFileName trashHome pd = do
  pathType <- case pd of
    PathDataDefault pd' -> pure (pd' ^. #pathType)
    PathDataFdo pd' -> Fdo.pathDataToType pd'
  PathType.deleteFn pathType trashPath'
  where
    MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)

-- | Returns the move function to be used with this PathData.
moveFn ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadThrow m
  ) =>
  PathData ->
  Path ->
  Path ->
  m ()
moveFn (PathDataDefault pd) x y = PathType.renameFn (pd ^. #pathType) x y
moveFn (PathDataFdo pd) x y = do
  ty <- Fdo.pathDataToType pd
  PathType.renameFn ty x y

-- | Header names.
--
-- @since 0.1
headerNames :: (Exts.IsList a, IsString (Exts.Item a)) => PathDataBackend -> a
headerNames PathDataBackendDefault = Default.headerNames
headerNames PathDataBackendFdo = Fdo.headerNames

-- | Normalizes the wrapper 'PathData' into the specific default backend's
-- 'Default.PathData'. This is so we can functionality that relies on this
-- more specific type e.g. formatting.
normalizeDefault ::
  ( HasCallStack,
    MonadLogger m,
    MonadPathReader m,
    MonadPathSize m,
    MonadTerminal m
  ) =>
  PathData ->
  m Default.PathData
normalizeDefault (PathDataDefault pd) = pure pd
normalizeDefault (PathDataFdo pd) = do
  isFile <- doesFileExist originalPath
  let pathType =
        if isFile
          then PathTypeFile
          else PathTypeDirectory

  size <-
    fmap (MkBytes @B) $
      pathSizeRecursive originalPath >>= \case
        PathSizeSuccess n -> pure n
        PathSizePartial errs n -> do
          -- We received a value but had some errors.
          putStrLn "Encountered errors retrieving size. See logs."
          for_ errs $ \e -> $(logError) (T.pack $ displayException e)
          pure n

  pure $
    Default.UnsafePathData
      { fileName = pd ^. #fileName,
        originalPath = pd ^. #originalPath,
        created = pd ^. #created,
        pathType,
        size
      }
  where
    MkPathI originalPath = pd ^. #originalPath

-- \| Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashPath :: PathI TrashHome -> PathData -> PathI TrashEntryPath
pathDataToTrashPath trashHome = Env.getTrashPath trashHome . view #fileName

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashInfoPath :: PathI TrashHome -> PathData -> PathI TrashEntryInfo
pathDataToTrashInfoPath trashHome = Env.getTrashInfoPath trashHome . view #fileName
