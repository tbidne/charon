-- | Provides the 'Serialize' class.
module SafeRm.Data.Serialize
  ( Serialize (..),
    encodeThrowM,
    decodeUnit,
  )
where

import SafeRm.Prelude

-- REVIEW: Consider renaming this class to disambiguate from the Serialise
-- class used with cbor e.g. SerializeTrash.

-- | Class for (de)serializing data.
class Serialize a where
  -- | Extra data used for decoding.
  type DecodeExtra a

  -- | Encode to bytestring.
  encode :: a -> Either String ByteString

  -- | Decode from a bytestring.
  decode :: DecodeExtra a -> ByteString -> Either String a

-- | Encodes the value, throwing an exception for any failures.
encodeThrowM :: (Serialize a, MonadThrow m) => a -> m ByteString
encodeThrowM x = case encode x of
  Left s -> throwString s
  Right y -> pure y

-- | Convenience function for when decoding takes no extra data.
decodeUnit :: (DecodeExtra a ~ (), Serialize a) => ByteString -> Either String a
decodeUnit = decode ()
