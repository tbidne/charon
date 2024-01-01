{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timestamp' data type.
module Charon.Data.Timestamp
  ( Timestamp (..),
    toText,
    fromText,
    toTextSpace,
  )
where

import Charon.Class.Serial (Serial (DecodeExtra, decode, encode))
import Charon.Prelude
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as Serialise
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Time (Day (ModifiedJulianDay), TimeOfDay (TimeOfDay))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime (LocalTime))

-- NOTE: We currently do not include any timezone information. We started
-- out doing so at first but then realized timezone parsing is unsatisfactory.
-- Parsing requires a locale, and the only one provided by the time package
-- (Format.defaultTimeLocale) is restricted to American timezones. The
-- time-conv package ostensibly fixes this, however it does not handle
-- daylight savings time i.e. parsing will fail on e.g. EDT, NZDT. Ideally we
-- would fix this upstream, though in the meantime we leave out timezone info
-- altogether.

-- | Represents a point in time.
newtype Timestamp = MkTimestamp
  { unTimestamp :: LocalTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)
  deriving (FromJSON, Hashable, ToJSON) via LocalTime

makeFieldLabelsNoPrefix ''Timestamp

instance Display Timestamp where
  displayBuilder = fromString . formatLocalTimeSpace . view #unTimestamp

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

-- This instance instance exists for backends that need a general "serialize
-- this timestamp" function without having any particular format. For instance,
-- fdo backend needs to write timestamps to a string, but cbor/json will have
-- their own strategies.
instance Serial Timestamp where
  type DecodeExtra Timestamp = ()
  encode = pure . encodeUtf8 . toText
  decode _ bs = case decodeUtf8 bs of
    Left err -> Left $ displayException err
    Right timeStr -> case parseLocalTime (T.unpack timeStr) of
      Nothing -> Left $ "Could not read time: " <> T.unpack timeStr
      Just t -> Right $ MkTimestamp t

-- | Formats the time.
toText :: Timestamp -> Text
toText = T.pack . formatLocalTime . view #unTimestamp

fromText :: (MonadFail f) => Text -> f Timestamp
fromText = fmap MkTimestamp . parseLocalTime . T.unpack

formatLocalTime :: LocalTime -> String
formatLocalTime = Format.formatTime Format.defaultTimeLocale localTimeFormat

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
toTextSpace :: Timestamp -> Text
toTextSpace = T.pack . formatLocalTimeSpace . view #unTimestamp

-- | Like 'toText' except adds a space between date and time. Used for
-- pretty-printing.
formatLocalTimeSpace :: LocalTime -> String
formatLocalTimeSpace =
  Format.formatTime Format.defaultTimeLocale localTimeFormatSpace

localTimeFormatSpace :: String
localTimeFormatSpace = "%0Y-%m-%d %H:%M:%S"
