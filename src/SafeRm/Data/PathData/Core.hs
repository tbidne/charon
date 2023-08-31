{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core 'PathData' for actions that require more fields like size (e.g.
-- formatting).
module SafeRm.Data.PathData.Core
  ( -- * PathData
    PathData (..),
    headerNames,
  )
where

import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath),
  )
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude
import SafeRm.Utils qualified as U

-- | Data for a path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The type of the path.
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | The size of the file or directory.
    size :: !(Bytes B Natural),
    -- | Time this entry was created.
    created :: !Timestamp
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''PathData

instance Pretty PathData where
  pretty pd = vsep strs
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> pretty (decodeOsToFpShowText $ pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (decodeOsToFpShowText $ pd ^. #originalPath % #unPathI),
          \x -> x <> ":     " <+> pretty (pd ^. #pathType),
          \x -> x <> ":     " <+> pretty (U.normalizedFormat $ pd ^. #size),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Name", "Original", "Type", "Size", "Created"]
