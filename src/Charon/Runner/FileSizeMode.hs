module Charon.Runner.FileSizeMode
  ( FileSizeMode (..),
    parseFileSizeMode,
  )
where

import Charon.Prelude
import Charon.Runner.Default (Default (def))
import Data.Bytes (Size (M), SomeSize)
import Data.Bytes qualified as Bytes
import Data.Char qualified as Ch
import Data.Text qualified as T
import GHC.Real (truncate)

-- | Determines what to do if the log file surpasses the given size
-- threshold.
data FileSizeMode
  = -- | Print a warning.
    FileSizeModeWarn (Bytes B Natural)
  | -- | Delete the file.
    FileSizeModeDelete (Bytes B Natural)
  deriving stock (Eq, Show)

instance Default FileSizeMode where
  def = FileSizeModeDelete $ Bytes.convert_ fiftyMb
    where
      fiftyMb = MkBytes @M 50

parseFileSizeMode :: (MonadFail m) => Text -> m FileSizeMode
parseFileSizeMode txt = do
  let (m, byteTxt) = T.break Ch.isSpace txt
  cons <- case m of
    "warn" -> pure FileSizeModeWarn
    "delete" -> pure FileSizeModeDelete
    bad -> fail $ "Unrecognized size-mode: " <> unpackText bad
  case parseByteText byteTxt of
    Right b -> pure $ cons b
    Left err -> fail $ "Could not parse size-mode size: " <> unpackText err

parseByteText :: Text -> Either Text (Bytes B Natural)
parseByteText txt =
  case Bytes.parse @(SomeSize Natural) txt of
    Right b -> Right $ Bytes.convert_ @_ @B b
    Left _ -> case Bytes.parse @(SomeSize Double) txt of
      Right b -> Right (truncate <$> Bytes.convert_ @_ @B b)
      Left err -> Left err
