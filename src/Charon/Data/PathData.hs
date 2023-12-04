{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core 'PathData'. This intended to be the 'main' type representing path
-- data. Actual backends are transformed to/from this type.
module Charon.Data.PathData
  ( -- * PathData
    PathData (..),
    headerNames,
    originalPathExists,
    isDirectory,
  )
where

import Charon.Data.PathType (PathTypeW)
import Charon.Data.PathType qualified as PathType
import Charon.Data.Paths
  ( PathI,
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath),
  )
import Charon.Data.Timestamp (Timestamp)
import Charon.Prelude
import Charon.Utils qualified as U
import Data.Text qualified as T
import Effects.FileSystem.PathReader (_PathTypeDirectory)
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts

-- | Data for a path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The type of the path.
    pathType :: PathTypeW,
    -- | The path to be used in the trash directory.
    fileName :: PathI TrashEntryFileName,
    -- | The original path on the file system.
    originalPath :: PathI TrashEntryOriginalPath,
    -- | The size of the file or directory. This is the __total__ size,
    -- i.e. in the case of a directory it is
    --
    --     size := size(dir) + size(dir contents)
    size :: Bytes B Natural,
    -- | Time this entry was created.
    created :: Timestamp
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''PathData

instance Pretty PathData where
  pretty pd = vsep strs
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> pretty (displayPath $ pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (displayPath $ pd ^. #originalPath % #unPathI),
          \x -> x <> ":     " <+> pretty (pd ^. #pathType),
          \x -> x <> ":     " <+> pretty (U.normalizedFormat $ pd ^. #size),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

      displayPath = T.pack . decodeOsToFpDisplayEx

-- | Header names.
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Name", "Original", "Type", "Size", "Created"]

-- | Returns 'True' if the 'PathData'\'s @originalPath@ corresponds to a real
-- path that exists.
originalPathExists ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  PathData ->
  m Bool
originalPathExists pd =
  PathType.existsFn (pd ^. #pathType) (pd ^. (#originalPath % #unPathI))

isDirectory :: PathData -> Bool
isDirectory = is (#pathType % #unPathTypeW % _PathTypeDirectory)
