-- | Formats the default 'PathData'.
module Charon.Data.PathData.Formatting
  ( -- * Types
    PathDataFormat (..),
    _FormatMultiline,
    _FormatTabular,
    _FormatSingleline,
    ColFormat (..),
    _ColFormatFixed,
    _ColFormatMax,
    Coloring (..),

    -- ** Sort
    Sort (..),
    readSort,
    sortFn,

    -- * Format functions

    -- ** Tabular
    formatTabularHeader,
    formatTabularHeaderColor,
    formatTabularRow,
    formatTabularRowColor,

    -- ** Tabular simple
    formatTabularSimpleHeader,
    formatTabularSimpleHeaderColor,
    formatTabularSimpleRow,
    formatTabularSimpleRowColor,

    -- ** Multiline
    formatMultiline,

    -- ** Singular
    formatSingleline,
    formatSinglelineColor,

    -- * Sorting
    sortCreatedName,
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

import Charon.Data.PathData (PathData)
import Charon.Data.Timestamp qualified as Timestamp
import Charon.Prelude
import Charon.Utils qualified as U
import Data.Char qualified as Ch
import Data.Text qualified as T
import FileSystem.OsPath qualified as OsPath
import System.Console.Pretty (Color)
import System.Console.Pretty qualified as CPretty

data ColFormat
  = -- | Fixed length format.
    ColFormatFixed Natural
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

-- | Whether we should color list command.
data Coloring
  = -- | Coloring on
    ColoringOn
  | -- | Coloring off
    ColoringOff
  | -- | Attempt to detect if the terminal support colors.
    ColoringDetect
  deriving stock (Eq, Show)

-- | Determines how to format a textual 'PathData'.
data PathDataFormat
  = -- | Formats each file on its own line.
    FormatMultiline
  | -- | Formats all fields on the same line.
    FormatTabular Coloring (Maybe ColFormat) (Maybe ColFormat)
  | FormatTabularSimple Coloring
  | -- | Formats each entry on a single line, no table.
    FormatSingleline Coloring
  deriving stock (Eq, Show)

_FormatMultiline :: Prism' PathDataFormat ()
_FormatMultiline =
  prism
    (const FormatMultiline)
    ( \x -> case x of
        FormatMultiline -> Right ()
        _ -> Left x
    )
{-# INLINE _FormatMultiline #-}

_FormatTabular :: Prism' PathDataFormat (Coloring, Maybe ColFormat, Maybe ColFormat)
_FormatTabular =
  prism
    (\(a, b, c) -> FormatTabular a b c)
    ( \x -> case x of
        FormatTabular a b c -> Right (a, b, c)
        _ -> Left x
    )
{-# INLINE _FormatTabular #-}

_FormatSingleline :: Prism' PathDataFormat Coloring
_FormatSingleline =
  prism
    FormatSingleline
    ( \x -> case x of
        FormatSingleline c -> Right c
        _ -> Left x
    )
{-# INLINE _FormatSingleline #-}

sortCreatedName :: PathData -> PathData -> Ordering
sortCreatedName x y = case sortCreated x y of
  EQ -> sortName x y
  other -> other

sortNameCreated :: PathData -> PathData -> Ordering
sortNameCreated x y = case sortName x y of
  EQ -> sortCreated x y
  other -> other

sortOriginalCreated :: PathData -> PathData -> Ordering
sortOriginalCreated x y = case sortOriginal x y of
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
sortName pd1 pd2 = case liftA2 (,) (OsPath.decode p1) (OsPath.decode p2) of
  -- NOTE: Decoding to string is the default ordering. If for some reason this
  -- fails, fall back to OsPaths' ordering.
  Right (s1, s2) -> mapOrd (fmap Ch.toLower) s1 s2
  Left _ -> p1 `compare` p2
  where
    p1 = pd1 ^. (#fileName % #unPathI)
    p2 = pd2 ^. (#fileName % #unPathI)

sortOriginal :: PathData -> PathData -> Ordering
sortOriginal = mapOrd (view #originalPath)

-- | Sorts by the name.
sortSize :: PathData -> PathData -> Ordering
sortSize = mapOrd (view #size)

mapOrd :: (Ord b) => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y

formatMultiline :: PathData -> Text
formatMultiline = U.renderPretty

formatSingleline :: PathData -> Text
formatSingleline = formatSingleline' id

formatSinglelineColor :: Color -> PathData -> Text
formatSinglelineColor = formatSingleline' . CPretty.color

formatSingleline' :: (Text -> Text) -> PathData -> Text
formatSingleline' f pd =
  f
    $ mconcat
      [ Timestamp.toTextSpace $ pd ^. #created,
        " ",
        T.pack $ decodeDisplayEx $ pd ^. #originalPath % #unPathI
      ]

formatTabularHeader :: Natural -> Natural -> Text
formatTabularHeader = formatTabularHeader' id

formatTabularHeaderColor :: Color -> Natural -> Natural -> Text
formatTabularHeaderColor = formatTabularHeader' . CPretty.color

formatTabularHeader' :: (Text -> Text) -> Natural -> Natural -> Text
formatTabularHeader' f nameLen origLen =
  f
    $ mconcat
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

formatTabularSimpleHeader :: Natural -> Natural -> Text
formatTabularSimpleHeader = formatTabularSimpleHeader' id

formatTabularSimpleHeaderColor :: Color -> Natural -> Natural -> Text
formatTabularSimpleHeaderColor = formatTabularSimpleHeader' . CPretty.color

formatTabularSimpleHeader' :: (Text -> Text) -> Natural -> Natural -> Text
formatTabularSimpleHeader' f idxLen origLen =
  f
    $ mconcat
      [ fixLen idxLen "Index",
        sep,
        fixLen formatCreatedLen "Created",
        sep,
        -- No need to pad the length here as this is the last column
        "Original",
        "\n",
        titleLen
      ]
  where
    totalLen = idxLen + formatCreatedLen + origLen + 6
    titleLen = T.replicate (fromIntegral totalLen) "-"

formatTabularSimpleRow :: Natural -> Natural -> PathData -> Text
formatTabularSimpleRow x = formatTabularSimpleRow' id x

formatTabularSimpleRowColor :: Color -> Natural -> Natural -> PathData -> Text
formatTabularSimpleRowColor c x = formatTabularSimpleRow' (CPretty.color c) x

formatTabularSimpleRow' :: (Text -> Text) -> Natural -> Natural -> PathData -> Text
formatTabularSimpleRow' f idxLen idx pd =
  f
    $ mconcat
      [ fixLen idxLen (showt idx),
        sep,
        Timestamp.toTextSpace $ pd ^. #created,
        sep,
        T.pack $ decodeDisplayEx $ pd ^. #originalPath % #unPathI
      ]

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
formatTabularRow = formatTabularRow' id

formatTabularRowColor :: Color -> Natural -> Natural -> PathData -> Text
formatTabularRowColor = formatTabularRow' . CPretty.color

formatTabularRow' :: (Text -> Text) -> Natural -> Natural -> PathData -> Text
formatTabularRow' f nameLen origLen pd =
  f
    $ mconcat
      [ fixLen' nameLen (decodeDisplayEx $ pd ^. #fileName % #unPathI),
        sep,
        fixLen' origLen (decodeDisplayEx $ pd ^. #originalPath % #unPathI),
        sep,
        paddedType (pd ^. (#pathType % #unPathTypeW)),
        sep,
        fixLen formatSizeLen (U.normalizedFormat $ pd ^. #size),
        sep,
        Timestamp.toTextSpace (pd ^. #created)
      ]
  where
    paddedType PathTypeFile = "F   "
    paddedType PathTypeDirectory = "D   "
    paddedType PathTypeSymbolicLink = "L   "
    paddedType PathTypeOther = "O   "

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

-- | How to sort the index list.
data Sort
  = -- | Sort by created timestamp.
    Created
  | -- | Sort by original path.
    OriginalPath
  | -- | Sort by name.
    Name
  | -- | Sort by size.
    Size
  deriving stock (Eq, Show)

readSort :: (MonadFail m) => Text -> m Sort
readSort "created" = pure Created
readSort "name" = pure Name
readSort "original" = pure OriginalPath
readSort "size" = pure Size
readSort other = fail $ "Unrecognized sort: " <> T.unpack other

sortFn :: Bool -> Sort -> PathData -> PathData -> Ordering
sortFn b = \case
  Created -> rev sortCreatedName
  Name -> rev sortNameCreated
  OriginalPath -> rev sortOriginalCreated
  Size -> rev sortSizeName
  where
    rev
      | b = sortReverse
      | otherwise = id
