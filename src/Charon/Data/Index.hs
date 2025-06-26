{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
module Charon.Data.Index
  ( Index (..),
    empty,

    -- * Formatting
    formatIndex,
    Sort (..),
    readSort,

    -- * Low level utils
    getColoring,
    formatIndex',
    fromList,
    insert,
    tabularSimpleNoSort,
  )
where

import Charon.Data.PathData (PathData)
import Charon.Data.PathData qualified as PathDataCore
import Charon.Data.PathData.Formatting
  ( ColFormat,
    Coloring (ColoringDetect, ColoringOff, ColoringOn),
    PathDataFormat
      ( FormatMultiline,
        FormatSingleline,
        FormatTabular,
        FormatTabularSimple
      ),
    Sort (Name, Size),
    readSort,
    _ColFormatFixed,
    _ColFormatMax,
  )
import Charon.Data.PathData.Formatting qualified as Formatting
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryPath),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Prelude
import Charon.Runner.Command.List (ListCmdP2)
import Data.Foldable (toList)
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Ord (Ord (max))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.System.Terminal (getTerminalWidth)
import Effects.System.Terminal qualified as Term
import FileSystem.OsPath qualified as OsPath
import GHC.Real (RealFrac (floor))
import System.Console.Pretty (Color (Blue, Green, Magenta))

type PathDataCore = PathDataCore.PathData

-- | Index that stores the trash data.
newtype Index = MkIndex
  { unIndex :: Seq (PathDataCore, PathI TrashEntryPath)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Index

-- | Empty index.
empty :: Index
empty = MkIndex mempty

-- | Formats the 'Index' in a pretty way.
formatIndex ::
  forall m.
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadTerminal m
  ) =>
  -- | List config
  ListCmdP2 ->
  -- | The index to format
  Index ->
  m Text
formatIndex listCmd idx =
  formatIndex' listCmd (view _1 <$> view #unIndex idx)

-- | Formats the 'Index' in a pretty way.
formatIndex' ::
  forall m.
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadTerminal m
  ) =>
  -- | List config
  ListCmdP2 ->
  -- | The index to format
  Seq PathDataCore ->
  m Text
formatIndex' listCmd idx = addNamespace "formatIndex" $ case listCmd ^. #format of
  FormatMultiline ->
    pure $ multiline (Formatting.sortFn (listCmd ^. #revSort) (listCmd ^. #sort)) idx
  FormatSingleline color -> do
    coloring <- getColoring color
    pure $ singleline coloring idx
  FormatTabular color nameFormat origFormat -> do
    coloring <- getColoring color

    -- We want to format the table such that we (concisely) display as
    -- much information as possible while trying to avoid ugly text wrapping
    -- (i.e. keep the table w/in the terminal width). This is complicated
    -- by the fact that fileNames / originalPath can be arbitrarily long,
    -- thus we have several heuristics.
    --
    -- First, background:
    --
    --   - The three fields pathType, size, and created all have a fixed length.
    --
    --   - The remaining two fields -- fileName and originalPath -- can be
    --     arbitrarily long but have a required minimum size.
    --
    --   - We try to automatically format the table within the terminal width,
    --     but provide options for overriding the fileName / originalPath
    --     lengths independently:
    --
    --       1. Set the column length to a specified length.
    --       2. Set the column length to the max row entry (e.g. longest
    --          fileName).
    --
    -- Now for the strategies:
    --
    --    1. If an option is explicitly set, use it, potential wrapping be
    --      damned. If the other field is unspecified, try to calculate the
    --      "best" option: i.e. one of the following strategies:
    --
    --        a. Use the max row entry if it fits within the terminal.
    --        b. If it doesn't fit, use remaining terminal space.
    --        c. If we don't have _any_ available space left -- according to the
    --           derived terminal space -- fallback to the required minimum.
    --
    --    2. If nothing is specified:
    --
    --       a. Try to display all data w/o truncation i.e. set each column
    --          to the max entry (e.g. longest fileName).
    --       b. If we cannot fit the maxes within the terminal width, use the
    --          remaining width according to the following ratio: 40% for
    --          fileNames, 60% for the originalPath.

    -- maxNameLen := longest name
    -- maxOrigLen := orig path
    (maxNameLen, maxOrigLen) <- foldl' findMaxes (pure maxStart) idx

    -- maxLen := maximum terminal width
    maxLen <- getTerminalLen

    -- maxLenForDynCols := available combined length for our dynamic
    -- columns (fileName + originalPath)
    maxLenForDynCols <-
      if maxLen < Formatting.minTableWidth
        then
          throwString
            $ mconcat
              [ "Terminal width (",
                show maxLen,
                ") is less than minimum width (",
                show Formatting.minTableWidth,
                ") for automatic tabular display.",
                " Perhaps try multiline."
              ]
        else pure $ maxLen - Formatting.reservedLineLen

    -- Basically: if an option is explicitly specified; use it. Otherwise,
    -- try to calculate a "good" value.
    (nameLen, origLen) <- case ( fmtToStrategy maxNameLen nameFormat,
                                 fmtToStrategy maxOrigLen origFormat
                               ) of
      -- Both set, use them.
      (Just nLen, Just oLen) -> pure (nLen, oLen)
      -- nLen set -> derive oLen
      (Just nLen, Nothing) ->
        (nLen,) -- (nLen, derived oLen)
          <$> mkDynamicLen
            maxLenForDynCols
            Formatting.formatOriginalPathLenMin
            maxOrigLen
            nLen
      -- oLen set -> derive nLen
      (Nothing, Just oLen) ->
        (,oLen) -- (derived nLen, oLen)
          <$> mkDynamicLen
            maxLenForDynCols
            Formatting.formatFileNameLenMin
            maxNameLen
            oLen
      -- Neither set; Use both maxes if they fit, otherwise approx.
      (Nothing, Nothing) ->
        if maxNameLen + maxOrigLen <= maxLenForDynCols
          then pure (maxNameLen, maxOrigLen)
          else
            let maxLenDynColsD = fromIntegral @_ @Double maxLenForDynCols
                nameApprox = max 4 (floor $ maxLenDynColsD * 0.4)
                origApprox = maxLenForDynCols - nameApprox
             in pure (nameApprox, origApprox)

    pure $ tabular coloring (Formatting.sortFn revSort sort) nameLen origLen idx
    where
      -- Search the index; find the longest name and orig path
      findMaxes :: m (Natural, Natural) -> PathDataCore -> m (Natural, Natural)
      findMaxes acc pd = do
        (!maxNameSoFar, !maxOrigSoFar) <- acc
        nameLen <- pathLen $ pd ^. #fileName
        origLen <- pathLen $ pd ^. #originalPath
        pure (max maxNameSoFar nameLen, max maxOrigSoFar origLen)

      maxStart :: (Natural, Natural)
      maxStart = (Formatting.formatFileNameLenMin, Formatting.formatOriginalPathLenMin)

      pathLen :: PathI i -> m Natural
      pathLen (MkPathI p) = do
        p' <- OsPath.decodeThrowM p
        pure $ fromIntegral $ length p'

      -- Map the format to its strategy
      fmtToStrategy :: Natural -> Maybe ColFormat -> Maybe Natural
      fmtToStrategy maxLen mcolFormat = wantsFixed <|> wantsMax
        where
          wantsFixed = preview (_Just % _ColFormatFixed) mcolFormat
          wantsMax = preview (_Just % _ColFormatMax) mcolFormat $> maxLen

      revSort = listCmd ^. #revSort
      sort = listCmd ^. #sort
  FormatTabularSimple color -> do
    coloring <- getColoring color
    pure $ tabularSimple coloring idx

-- | Derives the column lengths for our one dynamic column @C@ when given
-- exactly one fixed column length (@D_len@).
--
-- Given @mkDynamicLen T_max C_min C_max D_len@, we return @C_len@ based on
-- the following logic:
--
-- 1. If @D_len + C_max <= T_max@:
--        Return @C_max@.
-- 2. Else if @D_len < T_max@:
--        Return @(max (T_max - D_len) C_min)@.
--        That is, use @D_len@ as requested and give all remaining space to @C@
--        (falling back to @C_min@ if required).
-- 3. Otherwise:
--        Return @C_min@. We are going to wrap regardless since @D_len > T_max@,
--        so just ust it and @C_min@.
mkDynamicLen ::
  (MonadLoggerNS f) =>
  -- | @T_max@: Max total len for our dynamic columns
  Natural ->
  -- | @C_min@: Min required len for our derived column
  Natural ->
  -- | @C_max@: Max len for the column (i.e. the smallest @l@ s.t. all entries fit
  -- in this length)
  Natural ->
  -- | @D_len@: The fixed, requested length for the other column.
  Natural ->
  -- | Derived @C_len@
  f Natural
mkDynamicLen tMax cMin cMax dLen =
  addNamespace "mkDynamicLen"
    $ if dLen + cMax <= tMax
      then -- 1. dLen and cMax fit; use cMax
        pure cMax
      else do
        -- 2. dLen + cMax will not fit, but at least dLen < tMax; use all
        -- remaining space to print as much as we can. As we require at least
        -- minimums, this could lead to wrapping.
        if dLen < tMax
          then do
            $(logDebug)
              $ mconcat
                [ "Maximum len (",
                  showt cMax,
                  ") does not fit with requested other length (",
                  showt dLen,
                  ") and calculated terminal space (",
                  showt tMax,
                  ")"
                ]
            pure (max (tMax - dLen) cMin)
          else -- 3. Requested dLen > tMax. We are going to wrap regardless,
          -- so use cMin.
            do
              $(logWarn)
                $ mconcat
                  [ "Requested other length (",
                    showt dLen,
                    ") > calculated terminal space (",
                    showt tMax,
                    "). Falling back to minimum len: ",
                    showt cMin
                  ]
              pure cMin

getTerminalLen :: (MonadCatch m, MonadLoggerNS m, MonadTerminal m) => m Natural
getTerminalLen = addNamespace "getTerminalLen" $ do
  trySync getTerminalWidth >>= \case
    Right w -> pure w
    Left err -> do
      $(logWarn)
        $ "Could not detect terminal length. Falling back to default 80. Error:\n"
        <> displayExceptiont err
      pure 80

multiline :: (PathDataCore -> PathDataCore -> Ordering) -> Seq PathDataCore -> Text
multiline sort =
  T.intercalate "\n\n"
    . fmap Formatting.formatMultiline
    . toList
    . getElems sort

singleline :: Bool -> Seq PathDataCore -> Text
singleline coloring =
  T.intercalate "\n"
    . foldr f []
    . L.zip colorStream
    . toList
    . Seq.sortOn (view #originalPath)
  where
    f :: (Color, PathData) -> [Text] -> [Text]
    f (c, pd) acc = colorFn c pd : acc

    colorFn
      | coloring = Formatting.formatSinglelineColor
      | otherwise = const Formatting.formatSingleline

tabularSimple :: Bool -> Seq PathDataCore -> Text
tabularSimple coloring =
  tabularSimpleNoSort coloring
    . Seq.sortOn (view #originalPath)

tabularSimpleNoSort :: Bool -> Seq PathDataCore -> Text
tabularSimpleNoSort coloring xs =
  ((headerFn headerColor idxLen origLen <> "\n") <>)
    . T.intercalate "\n"
    . foldr f []
    . L.zip3 colorStream [1 ..]
    . toList
    $ xs
  where
    headerColor = Green

    f :: (Color, Natural, PathData) -> [Text] -> [Text]
    f (c, idx, pd) acc = rowFn c idxLen idx pd : acc

    (headerFn, rowFn)
      | coloring =
          ( Formatting.formatTabularSimpleHeaderColor,
            Formatting.formatTabularSimpleRowColor
          )
      | otherwise =
          ( const Formatting.formatTabularSimpleHeader,
            const Formatting.formatTabularSimpleRow
          )

    -- maximum of text 'Index' or string length of max index i.e. number
    -- + 1 for trailing colon e.g. '10:' (3).
    idxLen = fromIntegral $ max (length (show maxIdx) + 1) 5

    origLen = max maxOrig Formatting.formatOriginalPathLenMin

    maxOrig = fromIntegral $ F.maximum (Paths.pathLength . view #originalPath <$> xs)
    maxIdx = length xs

colorStream :: [Color]
colorStream = Blue : Magenta : colorStream

tabular ::
  Bool ->
  (PathDataCore -> PathDataCore -> Ordering) ->
  Natural ->
  Natural ->
  Seq PathDataCore ->
  Text
tabular coloring sort nameLen origLen =
  ((headerFn headerColor nameLen origLen <> "\n") <>)
    . T.intercalate "\n"
    . foldr f []
    . L.zip colorStream
    . toList
    . getElems sort
  where
    headerColor = Green

    f :: (Color, PathData) -> [Text] -> [Text]
    f (c, pd) acc = rowFn c nameLen origLen pd : acc

    (headerFn, rowFn)
      | coloring = (Formatting.formatTabularHeaderColor, Formatting.formatTabularRowColor)
      | otherwise = (const Formatting.formatTabularHeader, const Formatting.formatTabularRow)

getElems ::
  (PathDataCore -> PathDataCore -> Ordering) ->
  Seq PathDataCore ->
  Seq PathDataCore
getElems = Seq.sortBy

fromList :: [PathData] -> HashMap (PathI TrashEntryFileName) PathData
fromList = foldr insert HMap.empty

insert ::
  PathData ->
  HashMap (PathI TrashEntryFileName) PathData ->
  HashMap (PathI TrashEntryFileName) PathData
insert pd = HMap.insert (pd ^. #fileName) pd

getColoring :: (HasCallStack, MonadTerminal m) => Coloring -> m Bool
getColoring ColoringDetect = Term.supportsPretty
getColoring ColoringOff = pure False
getColoring ColoringOn = pure True
