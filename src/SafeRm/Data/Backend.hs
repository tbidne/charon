{-# LANGUAGE QuasiQuotes #-}

-- | Provides the 'Backend' data type.
module SafeRm.Data.Backend
  ( Backend (..),
    parseBackend,
    backendTestDesc,
    backendArg,
    backendArgOsPath,
  )
where

import Data.Text qualified as T
import SafeRm.Prelude
import TOML (DecodeTOML (..))

-- | Type of backend.
data Backend
  = -- | For use with the cbor backend.
    BackendCbor
  | -- | For use with the FreeDesktopOrg backend.
    BackendFdo
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML Backend where
  tomlDecoder = tomlDecoder >>= parseBackend

-- | Parses a 'PathDataBackend'.
parseBackend :: (MonadFail m) => Text -> m Backend
parseBackend "cbor" = pure BackendCbor
parseBackend "fdo" = pure BackendFdo
parseBackend other =
  fail
    $ mconcat
      [ "Could not parse backend: '",
        T.unpack other,
        "'. Expected 'cbor' or 'fdo'."
      ]

backendTestDesc :: Backend -> String
backendTestDesc BackendCbor = "(backend := cbor)"
backendTestDesc BackendFdo = "(backend := fdo)"

backendArg :: Backend -> String
backendArg BackendCbor = "cbor"
backendArg BackendFdo = "fdo"

backendArgOsPath :: Backend -> OsPath
backendArgOsPath BackendCbor = [osp|cbor|]
backendArgOsPath BackendFdo = [osp|fdo|]
