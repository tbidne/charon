-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.PathType
  ( PathType (..),
  )
where

import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Prelude

-- | Path type.
--
-- @since 0.1
data PathType
  = -- | File type.
    --
    -- @since 0.1
    PathTypeFile
  | -- | Directory type
    --
    -- @since 0.1
    PathTypeDirectory
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable,
      -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Pretty PathType where
  pretty PathTypeFile = "File"
  pretty PathTypeDirectory = "Directory"

-- | @since 0.1
instance Serialize PathType where
  type DecodeExtra PathType = ()
  encode PathTypeFile = "f"
  encode PathTypeDirectory = "d"

  decode _ "f" = Right PathTypeFile
  decode _ "d" = Right PathTypeDirectory
  decode _ other = Left $ "Could not decode path type: '" <> bsToStr other <> "'"
