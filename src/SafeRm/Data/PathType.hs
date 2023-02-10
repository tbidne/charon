-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.PathType
  ( PathType (..),
  )
where

import Data.Aeson qualified as Asn
import Data.Text qualified as T
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
instance FromJSON PathType where
  parseJSON = Asn.withText "PathType" $ \case
    "f" -> pure PathTypeFile
    "d" -> pure PathTypeDirectory
    bad -> fail $ "Expected path type 'f' or 'd', found: " <> T.unpack bad

-- | @since 0.1
instance ToJSON PathType where
  toJSON PathTypeFile = "f"
  toJSON PathTypeDirectory = "d"
