{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the FreeDesktopOrg backend.
module SafeRm.Data.PathData.Fdo
  ( -- * PathData
    PathData (..),
    toPathData,
    headerNames,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.HashSet qualified as Set
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI, PathIndex (..))
import SafeRm.Data.Serialize (Serialize (..), decodeUnit)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude

-- | Data for an Fdo path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The path to be used in the trash directory.
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | Time this entry was created.
    created :: !Timestamp
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''PathData

instance Pretty PathData where
  pretty pd = vsep strs <+> line
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> pretty (pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath % #unPathI),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Name", "Original", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
toPathData ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathType)
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
  (fileName', originalPath', pathType) <- Common.getPathInfo trashHome origPath

  pure
    ( UnsafePathData
        { fileName = fileName',
          originalPath = originalPath',
          created = currTime
        },
      pathType
    )

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> ByteString
  encode pd =
    -- FIXME: This needs to perform percent encoding:
    -- http://www.faqs.org/rfcs/rfc2396.html
    C8.unlines
      [ "[Trash Info]",
        "Path=" <> encode (pd ^. #originalPath),
        "DeletionDate=" <> encode (pd ^. #created)
      ]

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    mp <- Common.parseTrashInfoMap expectedKeys bs

    originalPath <- decodeUnit =<< Common.lookup "Path" mp
    created <- decodeUnit =<< Common.lookup "DeletionDate" mp

    Right $
      UnsafePathData
        { fileName = name,
          originalPath,
          created
        }
    where
      expectedKeys = Set.fromList ["Path", "DeletionDate"]
