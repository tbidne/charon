{-# LANGUAGE QuasiQuotes #-}

-- | Provides the 'Backend' data type.
module Charon.Backend.Data
  ( Backend (..),
    parseBackend,
    backendTestDesc,
    backendName,
    backendArgOsPath,
    backendExt,
  )
where

import Charon.Prelude
import Data.Text qualified as T
import TOML (DecodeTOML (tomlDecoder))

-- | Type of backend.
data Backend
  = -- | For use with the cbor backend.
    BackendCbor
  | -- | For use with the FreeDesktopOrg backend.
    BackendFdo
  | -- | For use with the json backend.
    BackendJson
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML Backend where
  tomlDecoder = tomlDecoder >>= parseBackend

-- | Parses a 'PathDataBackend'.
parseBackend :: (MonadFail m) => Text -> m Backend
parseBackend "cbor" = pure BackendCbor
parseBackend "fdo" = pure BackendFdo
parseBackend "json" = pure BackendJson
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
backendTestDesc BackendJson = "(backend := json)"

backendName :: (IsString s) => Backend -> s
backendName BackendCbor = "cbor"
backendName BackendFdo = "fdo"
backendName BackendJson = "json"

backendArgOsPath :: Backend -> OsPath
backendArgOsPath BackendCbor = [osp|cbor|]
backendArgOsPath BackendFdo = [osp|fdo|]
backendArgOsPath BackendJson = [osp|json|]

backendExt :: Backend -> OsPath
backendExt BackendCbor = [osp|.cbor|]
backendExt BackendFdo = [osp|.trashinfo|]
backendExt BackendJson = [osp|.json|]
