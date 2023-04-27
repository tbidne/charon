{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the default backend.
module SafeRm.Data.PathData.Default
  ( -- * PathData
    PathData (..),
    toPathData,
    headerNames,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.Paths (PathI, PathIndex (..))
import SafeRm.Data.Serialize (Serialize (..), decodeUnit)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude
import SafeRm.Utils qualified as U
import Text.Read qualified as TR

-- | Data for a path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The type of the path.
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | The size of the file or directory.
    size :: !(Bytes B Natural),
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
        [ \x -> x <> ":     " <+> pretty (pd ^. #pathType),
          \x -> x <> ":     " <+> pretty (pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath % #unPathI),
          \x -> x <> ":     " <+> pretty (U.normalizedFormat $ pd ^. #size),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Type", "Name", "Original", "Size", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
toPathData ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathType)
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
  (fileName', originalPath', pathType) <- Common.getPathInfo trashHome origPath

  size <-
    fmap (MkBytes @B) $
      pathSizeRecursive (originalPath' ^. #unPathI) >>= \case
        PathSizeSuccess n -> pure n
        PathSizePartial errs n -> do
          -- We received a value but had some errors.
          putStrLn "Encountered errors retrieving size. See logs."
          for_ errs $ \e -> $(logError) (T.pack $ displayException e)
          pure n

  pure
    ( UnsafePathData
        { fileName = fileName',
          originalPath = originalPath',
          pathType,
          size,
          created = currTime
        },
      pathType
    )

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> ByteString
  encode pd =
    C8.unlines
      [ "[Trash Info]",
        "Path=" <> encode (pd ^. #originalPath),
        "DeletionDate=" <> encode (pd ^. #created),
        "Size=" <> pd ^. (#size % _MkBytes % shown % packed % packedbs),
        "Type=" <> encode (pd ^. #pathType)
      ]

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    mp <- Common.parseTrashInfoMap expectedKeys bs

    originalPath <- decodeUnit =<< Common.lookup "Path" mp
    created <- decodeUnit =<< Common.lookup "DeletionDate" mp
    size <- decodeSize =<< Common.lookup "Size" mp
    pathType <- decodeUnit =<< Common.lookup "Type" mp

    Right $
      UnsafePathData
        { fileName = name,
          originalPath,
          pathType,
          size,
          created
        }
    where
      expectedKeys = Set.fromList ["Path", "DeletionDate", "Size", "Type"]

      decodeSize bs' = do
        let bytesStr = C8.unpack bs'
        case TR.readMaybe bytesStr of
          Nothing -> Left $ "Could not read bytes: " <> bytesStr
          Just n -> Right $ MkBytes n
