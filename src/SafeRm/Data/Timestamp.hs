{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timestamp' data type.
--
-- @since 0.1
module SafeRm.Data.Timestamp
  ( Timestamp (..),
    toText,
    fromText,
    toTextSpace,
  )
where

import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime (..))
import SafeRm.Data.Serialize (Serialize (..))
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
  pretty = fromString . formatLocalTimeSpace . view #unTimestamp

-- | @since 0.1
instance Serialize Timestamp where
  type DecodeExtra Timestamp = ()
  encode = encodeUtf8 . toText
  decode _ bs = case decodeUtf8 bs of
    Left err -> Left $ displayException err
    Right timeStr -> case parseLocalTime (T.unpack timeStr) of
      Nothing -> Left $ "Could not read time: " <> T.unpack timeStr
      Just t -> Right $ MkTimestamp t

-- | Formats the time.
--
-- @since 0.1
toText :: Timestamp -> Text
toText = T.pack . formatLocalTime . view #unTimestamp

-- | @since 0.1
fromText :: (MonadFail f) => Text -> f Timestamp
fromText = fmap MkTimestamp . parseLocalTime . T.unpack

-- | @since 0.1
formatLocalTime :: LocalTime -> String
formatLocalTime = Format.formatTime Format.defaultTimeLocale localTimeFormat

-- | @since 0.1
parseLocalTime :: (MonadFail f) => String -> f LocalTime
parseLocalTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat

localTimeFormat :: String
localTimeFormat = "%0Y-%m-%dT%H:%M:%S"

-- | Like 'toText' except adds a space between date and time. Used for
-- pretty-printing.
--
-- @since 0.1
toTextSpace :: Timestamp -> Text
toTextSpace = T.pack . formatLocalTimeSpace . view #unTimestamp

-- | Like 'toText' except adds a space between date and time. Used for
-- pretty-printing.
--
-- @since 0.1
formatLocalTimeSpace :: LocalTime -> String
formatLocalTimeSpace =
  Format.formatTime Format.defaultTimeLocale localTimeFormatSpace

localTimeFormatSpace :: String
localTimeFormatSpace = "%0Y-%m-%d %H:%M:%S"
