module Charon.Backend.Fdo.Utils
  ( percentEncodeFileName,
  )
where

import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashEntryFileName))
import Charon.Prelude
import Charon.Utils qualified as Utils
import Data.Text qualified as T

percentEncodeFileName ::
  forall m pd k.
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadThrow m
  ) =>
  pd ->
  m ByteString
percentEncodeFileName pd =
  percentEncode <$> decodeOsToFpThrowM fileName
  where
    percentEncode = Utils.percentEncode . encodeUtf8 . T.pack
    MkPathI fileName = view #fileName pd
