{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface for backend-agnostic 'PathData' operations. Modules should
-- generally use this instead of the backend-specific modules.
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
    convert,

    -- * Miscellaneous
    normalizeCore,
    pathDataToTrashPath,
    pathDataToTrashInfoPath,
  )
where

import Data.Bifunctor (first)
import Data.Text qualified as T
import PathSize (PathSizeResult (..), pathSizeRecursive)
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.PathData.Cbor qualified as Cbor
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathData.Core qualified as Core
import SafeRm.Data.PathData.Fdo qualified as Fdo
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.PathType qualified as PathType
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env (HasTrashHome (..))
import SafeRm.Env qualified as Env
import SafeRm.Prelude

-- | Holds information about a file path.
data PathData
  = PathDataCbor Cbor.PathData
  | PathDataFdo Fdo.PathData
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

instance Serialize PathData where
  type DecodeExtra PathData = (Backend, PathI TrashEntryFileName)
  encode (PathDataCbor pd) = encode pd
  encode (PathDataFdo pd) = encode pd

  decode (BackendCbor, fileName) bs = PathDataCbor <$> decode fileName bs
  decode (BackendFdo, fileName) bs = PathDataFdo <$> decode fileName bs

instance
  (k ~ A_Getter, a ~ PathI TrashEntryFileName, b ~ PathI TrashEntryFileName) =>
  LabelOptic "fileName" k PathData PathData a b
  where
  labelOptic = to getter
    where
      getter (PathDataCbor pd) = pd ^. #fileName
      getter (PathDataFdo pd) = pd ^. #fileName
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Getter, a ~ PathI TrashEntryOriginalPath, b ~ PathI TrashEntryOriginalPath) =>
  LabelOptic "originalPath" k PathData PathData a b
  where
  labelOptic = to getter
    where
      getter (PathDataCbor pd) = pd ^. #originalPath
      getter (PathDataFdo pd) = pd ^. #originalPath
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Getter, a ~ Timestamp, b ~ Timestamp) =>
  LabelOptic "created" k PathData PathData a b
  where
  labelOptic = to getter
    where
      getter (PathDataCbor pd) = pd ^. #created
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
toPathData ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Backend ->
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathType)
toPathData BackendCbor ts th = fmap (first PathDataCbor) . Cbor.toPathData ts th
toPathData BackendFdo ts th = fmap (first PathDataFdo) . Fdo.toPathData ts th

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
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

-- | Normalizes the wrapper 'PathData' into the specific default backend's
-- 'Default.PathData'. This is so we can functionality that relies on this
-- more specific type e.g. formatting.
normalizeCore ::
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadLogger m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  PathData ->
  m Core.PathData
normalizeCore pd = do
  let fileName = pd ^. #fileName
      originalPath = pd ^. #originalPath
      created = pd ^. #created

  trashHome <- asks getTrashHome
  pathType <- Common.pathDataToType trashHome pd

  let MkPathI path = Env.getTrashPath trashHome (pd ^. #fileName)

  size <-
    fmap (MkBytes @B)
      $ pathSizeRecursive path
      >>= \case
        PathSizeSuccess n -> pure n
        PathSizePartial errs n -> do
          -- We received a value but had some errors.
          putStrLn "Encountered errors retrieving size."
          for_ errs $ \e -> do
            let errMsg = T.pack $ displayException e
            putTextLn errMsg
            $(logError) errMsg
          pure n
        PathSizeFailure errs -> do
          putStrLn "Encountered errors retrieving size. Defaulting to 0. See logs."
          for_ errs $ \e -> do
            let errMsg = T.pack $ displayException e
            putTextLn errMsg
            $(logError) errMsg
          pure 0
  pure
    $ Core.UnsafePathData
      { fileName,
        originalPath,
        created,
        pathType,
        size
      }

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
pathDataToType trashHome (PathDataCbor pd) = Common.pathDataToType trashHome pd
pathDataToType trashHome (PathDataFdo pd) = Common.pathDataToType trashHome pd

-- \| Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
--
pathDataToTrashPath :: PathI TrashHome -> PathData -> PathI TrashEntryPath
pathDataToTrashPath trashHome = Env.getTrashPath trashHome . view #fileName

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
pathDataToTrashInfoPath :: PathI TrashHome -> Backend -> PathData -> PathI TrashEntryInfo
pathDataToTrashInfoPath trashHome backend =
  Env.getTrashInfoPath backend trashHome
    . view #fileName

-- | Converts the given 'PathData' to the corresponding 'Backend'. If they
-- are already in sync then this is a no-op.
convert :: PathData -> Backend -> PathData
convert pd@(PathDataCbor _) BackendCbor = pd
convert pd@(PathDataFdo _) BackendFdo = pd
convert pd@(PathDataCbor _) BackendFdo =
  PathDataFdo
    $ Fdo.UnsafePathData
      { fileName = pd ^. #fileName,
        originalPath = pd ^. #originalPath,
        created = pd ^. #created
      }
convert pd@(PathDataFdo _) BackendCbor =
  PathDataCbor
    $ Cbor.UnsafePathData
      { fileName = pd ^. #fileName,
        originalPath = pd ^. #originalPath,
        created = pd ^. #created
      }
