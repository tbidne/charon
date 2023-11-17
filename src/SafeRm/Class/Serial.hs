-- | Provides the 'Serial' class.
module SafeRm.Class.Serial
  ( Serial (..),
    encodeThrowM,
    decodeUnit,
  )
where

import SafeRm.Prelude

-- | Class for (de)serializing data. This differs from the Serialise class
-- in that Serialise is specifically used for the Cbor backend, whereas this
-- is meant as a general interface.
--
-- For instance, the Cbor PathData implements Serial in terms of its
-- Serialise (binary) instance. We then use the common Serial interface
-- when (en|de)coding to/from a file.
--
-- Thus this interface is primarily intended for the various PathDatas to
-- implement. Less commonly, however, there are some types that implement
-- Serial as a general to/from ByteString, e.g. Timestampe implements it,
-- which is used by the Fdo backend.
class Serial a where
  -- | Extra data used for decoding.
  type DecodeExtra a

  -- | Encode to bytestring.
  encode :: a -> Either String ByteString

  -- | Decode from a bytestring.
  decode :: DecodeExtra a -> ByteString -> Either String a

-- | Encodes the value, throwing an exception for any failures.
encodeThrowM :: (MonadThrow m, Serial a) => a -> m ByteString
encodeThrowM x = case encode x of
  Left s -> throwString s
  Right y -> pure y

-- | Convenience function for when decoding takes no extra data.
decodeUnit :: (DecodeExtra a ~ (), Serial a) => ByteString -> Either String a
decodeUnit = decode ()
