{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timestamp' data type.
--
-- @since 0.1
module SafeRm.Data.Timestamp
  ( Timestamp (..),
    toText,
    fromText,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.Csv (FromField (parseField), ToField (toField))
import Data.Text qualified as T
import Data.Time.LocalTime.Compat (LocalTime)
import Effects.Time (formatLocalTime, parseLocalTime)
import SafeRm.Prelude

-- NOTE: We currently do not include any timezone information. We started
-- out doing so at first but then realized timezone parsing is unsatisfactory.
-- Parsing requires a locale, and the only one provided by the time package
-- (Format.defaultTimeLocale) is restricted to American timezones. The
-- time-conv package ostensibly fixes this, however it does not handle
-- daylight savings time i.e. parsing will fail on e.g. EDT, NZDT. Ideally we
-- would fix this upstream, though in the meantime we leave out timezone info
-- altogether.

-- | Represents a point in time.
--
-- @since 0.1
newtype Timestamp = MkTimestamp
  { unTimestamp :: LocalTime
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )
  deriving
    ( -- | @since 0.1
      Hashable
    )
    via LocalTime

-- | @since 0.1
makeFieldLabelsNoPrefix ''Timestamp

-- | @since 0.1
instance Pretty Timestamp where
  pretty = fromString . formatLocalTime . view #unTimestamp

-- | @since 0.1
instance FromField Timestamp where
  parseField = fmap MkTimestamp . parseLocalTime . bs2Str

-- | @since 0.1
instance ToField Timestamp where
  toField = str2Bs . formatLocalTime . view #unTimestamp

-- | Formats the time.
--
-- @since 0.1
toText :: Timestamp -> Text
toText = T.pack . formatLocalTime . view #unTimestamp

-- | @since 0.1
fromText :: MonadFail f => Text -> f Timestamp
fromText = fmap MkTimestamp . parseLocalTime . T.unpack

bs2Str :: ByteString -> String
bs2Str = Char8.unpack . Char8.strip

str2Bs :: String -> ByteString
str2Bs = Char8.strip . Char8.pack
