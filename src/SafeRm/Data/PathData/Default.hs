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
import Data.HashMap.Strict qualified as Map
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
--
-- @since 0.1
data PathData = UnsafePathData
  { -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | The size of the file or directory.
    --
    -- @since 0.1
    size :: !(Bytes B Natural),
    -- | Time this entry was created.
    --
    -- @since 0.1
    created :: !Timestamp
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable,
      -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''PathData

-- | @since 0.1
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
--
-- @since 0.1
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Type", "Name", "Original", "Size", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- @since 0.1
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
  m PathData
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

  pure $
    UnsafePathData
      { fileName = fileName',
        originalPath = originalPath',
        pathType,
        size,
        created = currTime
      }

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
    case C8.lines bs of
      [] -> Left "Received empty pathdata"
      (h : rest) | isHeader h -> do
        let mp = Map.fromList (fmap U.breakEqBS rest)

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
      _ -> Left $ "Did not receive header [Trash Info]: " <> bsToStr bs
    where
      isHeader = (== "[Trash Info]")

      decodeSize bs' = do
        let bytesStr = C8.unpack bs'
        case TR.readMaybe bytesStr of
          Nothing -> Left $ "Could not read bytes: " <> bytesStr
          Just n -> Right $ MkBytes n
