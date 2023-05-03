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
import SafeRm.Data.PathData.Core (PathData)
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Prelude
import SafeRm.Utils qualified as U

data ColFormat
  = -- | Fixed length format.
    ColFormatFixed !Natural
  | -- | Format the column according to its longest entry, if possible.
    ColFormatMax
  deriving stock (Eq, Show)

_ColFormatFixed :: Prism' ColFormat Natural
_ColFormatFixed =
  prism
    ColFormatFixed
    ( \x -> case x of
        ColFormatFixed lbl -> Right lbl
        _ -> Left x
    )
{-# INLINE _ColFormatFixed #-}

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
data PathDataFormat
  = -- | Formats each file on its own line.
    FormatMultiline
  | -- | Formats all fields on the same line.
    FormatTabular !(Maybe ColFormat) !(Maybe ColFormat)
  deriving stock (Eq, Show)

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
sortCreated :: PathData -> PathData -> Ordering
sortCreated = mapOrd (view #created)

-- | Sorts by the name.
sortName :: PathData -> PathData -> Ordering
sortName = mapOrd (fmap Ch.toLower . view (#fileName % #unPathI))

-- | Sorts by the name.
sortSize :: PathData -> PathData -> Ordering
sortSize = mapOrd (view #size)

mapOrd :: (Ord b) => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y

formatMultiLine :: PathData -> Text
formatMultiLine = U.renderPretty

formatTabularHeader :: Natural -> Natural -> Text
formatTabularHeader nameLen origLen =
  mconcat
    [ fixLen nameLen "Name",
      sep,
      fixLen origLen "Original",
      sep,
      fixLen formatTypeLen "Type",
      sep,
      fixLen formatSizeLen "Size",
      sep,
      -- No need to pad the length here as this is the last column
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
reservedLineLen :: Natural
reservedLineLen =
  formatTypeLen + formatSizeLen + formatCreatedLen + formatSeparatorsLen

-- | Minimum length needed to display the table.
minTableWidth :: Natural
minTableWidth =
  reservedLineLen + formatFileNameLenMin + formatOriginalPathLenMin

formatTabularRow :: Natural -> Natural -> PathData -> Text
formatTabularRow nameLen origLen pd =
  mconcat
    [ fixLen' nameLen (pd ^. #fileName % #unPathI),
      sep,
      fixLen' origLen (pd ^. #originalPath % #unPathI),
      sep,
      paddedType (pd ^. #pathType),
      sep,
      fixLen formatSizeLen (U.normalizedFormat $ pd ^. #size),
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

fixLen :: Natural -> Text -> Text
fixLen w t
  | w' < T.length t = T.take (w' - 3) t <> "..."
  | otherwise = t <> T.replicate (w' - T.length t) " "
  where
    w' = fromIntegral w

formatTypeLen :: Natural
formatTypeLen = 4

formatFileNameLenMin :: Natural
formatFileNameLenMin = 4

formatOriginalPathLenMin :: Natural
formatOriginalPathLenMin = 8

formatSizeLen :: Natural
formatSizeLen = 7

formatCreatedLen :: Natural
formatCreatedLen = 19

formatSeparatorsLen :: Natural
formatSeparatorsLen = 12
