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

instance Display PathData where
  displayBuilder pd = vsep strs
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> displayBuilder (pd ^. #fileName),
          \x -> x <> ": " <+> displayBuilder (pd ^. #originalPath),
          \x -> x <> ":     " <+> displayBuilder (pd ^. #pathType),
          \x -> x <> ":     " <+> displayBuilder (U.normalizedFormat $ pd ^. #size),
          \x -> x <> ":  " <+> displayBuilder (pd ^. #created)
        ]

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
isDirectory pd = case pd ^. #pathType % #unPathTypeW of
  PathTypeDirectory -> True
  _ -> False
