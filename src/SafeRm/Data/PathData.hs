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

    -- * Creation
    toPathData,

    -- * Elimination
    deleteFileName,

    -- * Existence
    trashPathExists,
    originalPathExists,

    -- * Operations
    pathDataToType,

    -- * Miscellaneous
    headerNames,
    normalizeDefault,
    pathDataToTrashPath,
    pathDataToTrashInfoPath,
  )
where

import Data.Bifunctor (first)
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Exts qualified as Exts
import SafeRm.Data.Backend (Backend (..))
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
  deriving anyclass (Hashable, NFData)

instance Pretty PathData where
  pretty (PathDataDefault pd) = pretty pd
  pretty (PathDataFdo pd) = pretty pd

instance Serialize PathData where
  type DecodeExtra PathData = (Backend, PathI TrashEntryFileName)
  encode (PathDataDefault pd) = encode pd
  encode (PathDataFdo pd) = encode pd

  decode (BackendDefault, fileName) bs = PathDataDefault <$> decode fileName bs
  decode (BackendFdo, fileName) bs = PathDataFdo <$> decode fileName bs

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

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * Created time.
-- * Any extra backend-specific fields (i.e. path type and size for default).
--
-- Additionally, we explicitly return the PathType. We do this because
-- the PathType does not exist on all backends (e.g. fdo) yet we need it upon
-- creation for choosing the correct move function (renameFile vs.
-- renameDirectory). We cannot rely on pathDataToType as that function is
-- only valid when the PathData entry has already been created in the trash.
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
  Backend ->
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathType)
toPathData BackendDefault ts th = fmap (first PathDataDefault) . Default.toPathData ts th
toPathData BackendFdo ts th = fmap (first PathDataFdo) . Fdo.toPathData ts th

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists th pd = doesPathExist trashPath'
  where
    -- NOTE: doesPathExist rather than doesFile/Dir... as that requires knowing
    -- the path type. See Note [PathData PathType conditions].

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
  PathI TrashHome ->
  PathData ->
  m Bool
originalPathExists th pd = do
  -- See Note [PathData PathType conditions].
  pathType <- pathDataToType th pd

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
  -- See Note [PathData PathType conditions].
  pathType <- pathDataToType trashHome pd
  PathType.deleteFn pathType trashPath'
  where
    MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)

-- | Header names.
--
-- @since 0.1
headerNames :: (Exts.IsList a, IsString (Exts.Item a)) => Backend -> a
headerNames BackendDefault = Default.headerNames
headerNames BackendFdo = Fdo.headerNames

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

-- | Returns the path type.
--
-- NOTE: [PathData PathType conditions]
--
-- __IMPORTANT:__ This function is only guaranteed to work if the 'PathData'
-- corresponds to an extant trash entry. In particular, if the 'PathData' has
-- not been created yet, this can fail with the fdo backend.
--
-- Also, this function should not be used when we are testing to see if a
-- a path exists e.g. to see if a path exists in both files/ and info/.
--
-- For instance, we want to give a precise error message when a path p exists
-- in info/ but not files/. Notice this requires successfully testing the
-- existence of both files/p and info/p.trashinfo. If we use pathDataToType
-- to grab the "right" existence function (doesFileExist vs. doesDirectoryExist),
-- then we will inadvertently be performing the existence check here, with one
-- key difference: we throw an exception rather than return a Bool! This will
-- in turn cause the error to degrade to a basic FileNotFound, obviously not
-- what we want.
pathDataToType ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m PathType
pathDataToType _ (PathDataDefault pd) = pure $ pd ^. #pathType
pathDataToType trashHome (PathDataFdo pd) = Fdo.pathDataToType trashHome pd

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
