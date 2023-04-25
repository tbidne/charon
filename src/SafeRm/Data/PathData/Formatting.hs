-- | Formats the default 'PathData'.
module SafeRm.Data.PathData.Formatting
  ( -- * Types
    PathDataFormat (..),
    ColFormat (..),

    -- * Format functions
    formatTabularHeader,
    formatTabularRow,
    formatMultiLine,

    -- * Sorting
    sortNameCreated,
    sortSizeName,
    sortReverse,

    -- * Field lengths
    formatFileNameLenMin,
    formatOriginalPathLenMin,
    minTableWidth,
    reservedLineLen,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import SafeRm.Data.PathData.Default (PathData)
import SafeRm.Data.PathData.Default qualified as Default
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Prelude
import SafeRm.Utils qualified as U

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
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

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

-- | @since 0.1
formatMultiLine :: PathData -> Text
formatMultiLine pd = T.intercalate "\n" strs
  where
    strs = zipWith (flip ($)) Default.headerNames labelFn
    labelFn =
      [ \x -> x <> ":     " <> typeToText (pd ^. #pathType),
        \x -> x <> ":     " <> T.pack (pd ^. #fileName % #unPathI),
        \x -> x <> ": " <> T.pack (pd ^. #originalPath % #unPathI),
        \x -> x <> ":     " <> U.normalizedFormat (pd ^. #size),
        \x -> x <> ":  " <> Timestamp.toTextSpace (pd ^. #created)
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
      Timestamp.toTextSpace (pd ^. #created)
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
