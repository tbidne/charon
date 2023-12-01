{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides metadata functionality.
module Charon.Data.Metadata
  ( Metadata (..),
    empty,
  )
where

import Charon.Prelude
import Charon.Utils qualified as U
import Data.Bytes (SomeSize)
import Numeric.Algebra (AMonoid (zero))

-- | Holds trash metadata.
data Metadata = MkMetadata
  { -- | Number of top level entries in the trash index. This should be the
    -- same as the index length.
    numEntries :: Natural,
    -- | Number of total files in the trash.
    numFiles :: Natural,
    -- | Size of the log file.
    logSize :: SomeSize Double,
    -- | Total size of the trash directory.
    size :: SomeSize Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Metadata

-- | Empty metadata.
empty :: Metadata
empty = MkMetadata 0 0 zero zero

instance Pretty Metadata where
  pretty stats = vsep strs <+> line
    where
      strs =
        [ "Entries:     " <+> pretty (stats ^. #numEntries),
          "Total Files: " <+> pretty (stats ^. #numFiles),
          "Log size:    " <+> pretty (U.formatBytes $ stats ^. #logSize),
          "Size:        " <+> pretty (U.formatBytes $ stats ^. #size)
        ]
