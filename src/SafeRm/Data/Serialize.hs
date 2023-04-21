-- | Provides the 'Serialize' class.
module SafeRm.Data.Serialize
  ( Serialize (..),
    decodeUnit,
  )
where

import SafeRm.Prelude

-- | Class for (de)serializing data.
class Serialize a where
  -- | Extra data used for decoding.
  type DecodeExtra a

  -- | Encode to bytestring.
  encode :: a -> ByteString

  -- | Decode from a bytestring.
  decode :: DecodeExtra a -> ByteString -> Either String a

-- | Convenience function for when decoding takes no extra data.
decodeUnit :: (DecodeExtra a ~ (), Serialize a) => ByteString -> Either String a
decodeUnit = decode ()
