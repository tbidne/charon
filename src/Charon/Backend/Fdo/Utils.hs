module Charon.Backend.Fdo.Utils
  ( percentEncodeFileName,
  )
where

import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashEntryFileName))
import Charon.Prelude
import Charon.Utils qualified as Utils
import Data.Text qualified as T
import FileSystem.OsPath qualified as OsPath

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
  percentEncode =<< OsPath.decodeThrowM fileName
  where
    percentEncode =
      throwLeft
        . first MkStringException
        . Utils.percentEncode
        . encodeUtf8
        . T.pack
    MkPathI fileName = view #fileName pd
