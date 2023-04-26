{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the FreeDesktopOrg backend.
module SafeRm.Data.PathData.Fdo
  ( -- * PathData
    PathData (..),
    toPathData,
    headerNames,

    -- * Misc
    pathDataToType,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Serialize (Serialize (..), decodeUnit)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env qualified as Env
import SafeRm.Exception (FileNotFoundE (..), PathNotFileDirE (..))
import SafeRm.Prelude
import SafeRm.Utils qualified as U

-- | Data for an Fdo path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The path to be used in the trash directory.
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | Time this entry was created.
    created :: !Timestamp
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''PathData

instance Pretty PathData where
  pretty pd = vsep strs <+> line
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> pretty (pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath % #unPathI),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Name", "Original", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
toPathData ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathType)
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
  (fileName', originalPath', pathType) <- Common.getPathInfo trashHome origPath

  pure
    ( UnsafePathData
        { fileName = fileName',
          originalPath = originalPath',
          created = currTime
        },
      pathType
    )

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> ByteString
  encode pd =
    C8.unlines
      [ "[Trash Info]",
        "Path=" <> encode (pd ^. #originalPath),
        "DeletionDate=" <> encode (pd ^. #created)
      ]

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    case C8.lines bs of
      [] -> Left "Received empty pathdata"
      (h : rest) | isHeader h -> do
        let mp = Map.fromList (fmap U.breakEqBS rest)
            keys = Map.keysSet mp
            expectedKeys = Set.fromList ["Path", "DeletionDate"]
            unexpectedKeys = Set.difference keys expectedKeys
            unexpectedKeysStr = C8.intercalate ", " $ Set.toList unexpectedKeys

        unless (Set.null unexpectedKeys) $
          Left $
            "Unexpected keys: " <> bsToStrLenient unexpectedKeysStr

        originalPath <- decodeUnit =<< Common.lookup "Path" mp
        created <- decodeUnit =<< Common.lookup "DeletionDate" mp

        Right $
          UnsafePathData
            { fileName = name,
              originalPath,
              created
            }
      _ -> Left $ "Did not receive header [Trash Info]: " <> bsToStr bs
    where
      isHeader = (== "[Trash Info]")

-- | Derives the 'PathType' from the 'PathData'.
--
-- __IMPORTANT:__ This function is only guaranteed to work if the 'PathData'
-- corresponds to an extant trash entry. In particular, if the 'PathData' has
-- not been created yet, this can fail.
pathDataToType ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m PathType
pathDataToType trashHome pd = do
  fileExists <- doesFileExist path
  if fileExists
    then pure PathTypeFile
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          -- for a better error message
          pathExists <- doesPathExist path
          if pathExists
            then throwCS $ MkPathNotFileDirE path
            else throwCS $ MkFileNotFoundE path
  where
    MkPathI path = Env.getTrashPath trashHome (pd ^. #fileName)
