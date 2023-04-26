-- | Provides the 'Backend' data type.
module SafeRm.Data.Backend
  ( Backend (..),
    parseBackend,
    backendTestDesc,
    backendArg,
  )
where

import Data.Text qualified as T
import SafeRm.Prelude
import TOML (DecodeTOML (..))

-- | Type of backend.
data Backend
  = -- | For use with the default backend.
    BackendDefault
  | -- | For use with the FreeDesktopOrg backend.
    BackendFdo
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML Backend where
  tomlDecoder = tomlDecoder >>= parseBackend

-- | Parses a 'PathDataBackend'.
parseBackend :: (MonadFail m) => Text -> m Backend
parseBackend "default" = pure BackendDefault
parseBackend "fdo" = pure BackendFdo
parseBackend other =
  fail $
    mconcat
      [ "Could not parse backend: '",
        T.unpack other,
        "'. Expected 'default' or 'fdo'."
      ]

backendTestDesc :: Backend -> String
backendTestDesc BackendDefault = "(backend := default)"
backendTestDesc BackendFdo = "(backend := fdo)"

backendArg :: Backend -> String
backendArg BackendDefault = "default"
backendArg BackendFdo = "fdo"
