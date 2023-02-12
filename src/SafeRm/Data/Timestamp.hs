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
  )
where

import Codec.Serialise qualified as Serialise
import Data.Text qualified as T
import Data.Time (Day (..))
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
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
instance Serialise Timestamp where
  encode (MkTimestamp (LocalTime (ModifiedJulianDay d) (TimeOfDay h m s))) =
    mconcat
      [ Serialise.encode d,
        Serialise.encode h,
        Serialise.encode m,
        Serialise.encode s
      ]
  decode =
    (\d h m s -> MkTimestamp $ LocalTime (ModifiedJulianDay d) (TimeOfDay h m s))
      <$> Serialise.decode
      <*> Serialise.decode
      <*> Serialise.decode
      <*> Serialise.decode

-- | @since 0.1
instance Pretty Timestamp where
  pretty = fromString . formatLocalTime . view #unTimestamp

-- | Formats the time.
--
-- @since 0.1
toText :: Timestamp -> Text
toText = T.pack . formatLocalTime . view #unTimestamp

-- | @since 0.1
fromText :: MonadFail f => Text -> f Timestamp
fromText = fmap MkTimestamp . parseLocalTime . T.unpack
