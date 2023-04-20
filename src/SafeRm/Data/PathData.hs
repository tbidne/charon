{-# LANGUAGE OverloadedLists #-}

-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.PathData
  ( PathData,

    -- * Creation
    Internal.toPathData,
    Internal.decode,

    -- * Elimination
    Internal.encode,
    getRenameFn,
    getDeleteFn,

    -- * Existence
    trashPathExists,
    originalPathExists,
    getExistsFn,

    -- * Sorting

    -- ** High level
    sortCreatedName,
    sortNameCreated,
    sortSizeName,

    -- ** Low level
    sortReverse,
    sortCreated,
    sortName,
    sortSize,

    -- * Formatting

    -- ** Types
    PathDataFormat (..),
    ColFormat (..),
    _ColFormatFixed,
    _ColFormatMax,
    -- _ColFormatAuto,

    -- ** Functions
    formatMultiLine,
    formatTabularHeader,
    formatTabularRow,

    -- ** Field lengths
    minTableWidth,
    reservedLineLen,
    formatFileNameLenMin,
    formatOriginalPathLenMin,
    formatTypeLen,
    formatSizeLen,
    formatCreatedLen,
    formatSeparatorsLen,

    -- * Miscellaneous
    Internal.headerNames,
    pathDataToTrashPath,
    pathDataToTrashInfoPath,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import Effects.FileSystem.PathWriter (removeFile)
import SafeRm.Data.PathData.Internal (PathData)
import SafeRm.Data.PathData.Internal qualified as Internal
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Timestamp (toText)
import SafeRm.Env qualified as Env
import SafeRm.Prelude
import SafeRm.Utils qualified as U

-- | Sorts by the created date then the name.
--
-- @since 0.1
sortCreatedName :: PathData -> PathData -> Ordering
sortCreatedName x y = case sortCreated x y of
  EQ -> sortName x y
  other -> other

sortNameCreated :: PathData -> PathData -> Ordering
sortNameCreated x y = case sortName x y of
  EQ -> sortCreated x y
  other -> other

sortSizeName :: PathData -> PathData -> Ordering
sortSizeName x y = case sortSize x y of
  EQ -> sortName x y
  other -> other

sortReverse :: (a -> b -> Ordering) -> a -> b -> Ordering
sortReverse f x y = case f x y of
  EQ -> EQ
  LT -> GT
  GT -> LT

-- | Sorts by the created date.
--
-- @since 0.1
sortCreated :: PathData -> PathData -> Ordering
sortCreated = mapOrd (view #created)

-- | Sorts by the name.
--
-- @since 0.1
sortName :: PathData -> PathData -> Ordering
sortName = mapOrd (fmap Ch.toLower . view (#fileName % #unPathI))

-- | Sorts by the name.
--
-- @since 0.1
sortSize :: PathData -> PathData -> Ordering
sortSize = mapOrd (view #size)

mapOrd :: (Ord b) => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists ::
  (MonadPathReader m, HasCallStack) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists trashHome pd = existsFn trashPath'
  where
    MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)
    existsFn = case pd ^. #pathType of
      PathTypeFile -> doesFileExist
      PathTypeDirectory -> doesDirectoryExist

-- | Returns 'True' if the 'PathData'\'s @originalPath@ corresponds to a real
-- path that exists.
--
-- @since 0.1
originalPathExists ::
  (MonadPathReader m, HasCallStack) =>
  PathData ->
  m Bool
originalPathExists pd = existsFn (pd ^. #originalPath % #unPathI)
  where
    existsFn = case pd ^. #pathType of
      PathTypeFile -> doesFileExist
      PathTypeDirectory -> doesDirectoryExist

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashPath :: PathI TrashHome -> PathData -> PathI TrashEntryPath
pathDataToTrashPath trashHome = Env.getTrashPath trashHome . view #fileName

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashInfoPath :: PathI TrashHome -> PathData -> PathI TrashEntryInfo
pathDataToTrashInfoPath trashHome = Env.getTrashInfoPath trashHome . view #fileName

-- | @since 0.1
data ColFormat
  = -- | Fixed length format.
    --
    -- @since 0.1
    ColFormatFixed !Natural
  | -- | Format the column according to its longest entry, if possible.
    --
    -- @since 0.1
    ColFormatMax
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_ColFormatFixed :: Prism' ColFormat Natural
_ColFormatFixed =
  prism
    ColFormatFixed
    ( \x -> case x of
        ColFormatFixed lbl -> Right lbl
        _ -> Left x
    )
{-# INLINE _ColFormatFixed #-}

-- | @since 0.1
_ColFormatMax :: Prism' ColFormat ()
_ColFormatMax =
  prism
    (const ColFormatMax)
    ( \x -> case x of
        ColFormatMax -> Right ()
        _ -> Left x
    )
{-# INLINE _ColFormatMax #-}

-- | Determines how to format a textual 'PathData'.
--
-- @since 0.1
data PathDataFormat
  = -- | Formats each file on its own line.
    --
    -- @since 0.1
    FormatMultiline
  | -- | Formats all fields on the same line.
    --
    -- @since 0.1
    FormatTabular !(Maybe ColFormat) !(Maybe ColFormat)
  -- \| -- | Like 'FormatTabular', except it attempts to detect the best
  --   -- column widths automatically.
  --   --
  --   -- @since 0.1
  --   FormatTabularAuto
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
formatMultiLine :: PathData -> Text
formatMultiLine pd = T.intercalate "\n" strs
  where
    strs = zipWith (flip ($)) Internal.headerNames labelFn
    labelFn =
      [ \x -> x <> ":     " <> typeToText (pd ^. #pathType),
        \x -> x <> ":     " <> T.pack (pd ^. #fileName % #unPathI),
        \x -> x <> ": " <> T.pack (pd ^. #originalPath % #unPathI),
        \x -> x <> ":     " <> U.normalizedFormat (pd ^. #size),
        \x -> x <> ":  " <> toText (pd ^. #created)
      ]

typeToText :: PathType -> Text
typeToText PathTypeFile = "File"
typeToText PathTypeDirectory = "Directory"

-- | @since 0.1
formatTabularHeader :: Natural -> Natural -> Text
formatTabularHeader nameLen origLen =
  mconcat
    [ fixLen nameLen "Name",
      sep,
      fixLen formatTypeLen "Type",
      sep,
      fixLen formatSizeLen "Size",
      sep,
      fixLen origLen "Original",
      sep,
      -- No need to fix the length here as Created is the last column
      "Created",
      "\n",
      titleLen
    ]
  where
    -- extra 12 is from the separators
    totalLen = nameLen + origLen + reservedLineLen
    titleLen = T.replicate (fromIntegral totalLen) "-"

-- | For tabular formatting, this is the necessary width for the fixed
-- columns:
--
-- type: 9
-- size: 7
-- created: 19
-- separators: 12
--
-- NOTE: The separators includes the adjacent whitespace i.e. one separator
-- ' | ' counts for 3, and since we have 4 that makes 12.
--
-- This does not include the minimum necessary total space (i.e. minimum
-- 4 for name and 8 for original).
--
-- @since 0.1
reservedLineLen :: Natural
reservedLineLen =
  formatTypeLen + formatSizeLen + formatCreatedLen + formatSeparatorsLen

-- | Minimum length needed to display the table.
--
-- @since 0.1
minTableWidth :: Natural
minTableWidth =
  reservedLineLen + formatFileNameLenMin + formatOriginalPathLenMin

-- | @since 0.1
formatTabularRow :: Natural -> Natural -> PathData -> Text
formatTabularRow nameLen origLen pd =
  mconcat
    [ fixLen' nameLen (pd ^. #fileName % #unPathI),
      sep,
      paddedType (pd ^. #pathType),
      sep,
      fixLen formatSizeLen (U.normalizedFormat $ pd ^. #size),
      sep,
      fixLen' origLen (pd ^. #originalPath % #unPathI),
      sep,
      toText (pd ^. #created)
    ]
  where
    paddedType PathTypeFile = "F   "
    paddedType PathTypeDirectory = "D   "

sep :: Text
sep = " | "

fixLen' :: Natural -> String -> Text
fixLen' w s = fixLen w (T.pack s)

-- | @since 0.1
fixLen :: Natural -> Text -> Text
fixLen w t
  | w' < T.length t = T.take (w' - 3) t <> "..."
  | otherwise = t <> T.replicate (w' - T.length t) " "
  where
    w' = fromIntegral w

-- | @since 0.1
getRenameFn :: (MonadPathWriter m) => PathData -> Path -> Path -> m ()
getRenameFn pd = case pd ^. #pathType of
  PathTypeFile -> renameFile
  PathTypeDirectory -> renameDirectory

-- | @since 0.1
getDeleteFn :: (MonadPathWriter m) => PathData -> Path -> m ()
getDeleteFn pd = case pd ^. #pathType of
  PathTypeFile -> removeFile
  PathTypeDirectory -> removeDirectoryRecursive

-- | @since 0.1
getExistsFn :: (MonadPathReader m) => PathData -> Path -> m Bool
getExistsFn pd = case pd ^. #pathType of
  PathTypeFile -> doesFileExist
  PathTypeDirectory -> doesDirectoryExist

-- | @since 0.1
formatTypeLen :: Natural
formatTypeLen = 4

-- | @since 0.1
formatFileNameLenMin :: Natural
formatFileNameLenMin = 4

-- | @since 0.1
formatOriginalPathLenMin :: Natural
formatOriginalPathLenMin = 8

-- | @since 0.1
formatSizeLen :: Natural
formatSizeLen = 7

-- | @since 0.1
formatCreatedLen :: Natural
formatCreatedLen = 19

-- | @since 0.1
formatSeparatorsLen :: Natural
formatSeparatorsLen = 12
