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
    formatIndex',
    fromList,
    insert,
  )
where

import Charon.Data.PathData (PathData)
import Charon.Data.PathData qualified as PathDataCore
import Charon.Data.PathData.Formatting
  ( ColFormat (ColFormatFixed, ColFormatMax),
    Coloring (ColoringDetect, ColoringOff, ColoringOn),
    PathDataFormat (FormatMultiline, FormatSingleline, FormatTabular),
    Sort (Name, Size),
    readSort,
  )
import Charon.Data.PathData.Formatting qualified as Formatting
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryPath),
  )
import Charon.Prelude
import Charon.Runner.Command.List (ListCmdP2)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Ord (Ord (max))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.System.Terminal (getTerminalWidth)
import Effects.System.Terminal qualified as Term
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
    (maxNameLen, maxOrigLen) <- foldl' foldMap (pure maxStart) idx

    -- maxLen := maximum terminal width
    maxLen <- getMaxLen

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
    (nameLen, origLen) <- case ( nameFmtToStrategy maxNameLen nameFormat,
                                 origFmtToStrategy maxOrigLen origFormat
                               ) of
      -- Both set, use them.
      (Right nLen, Right oLen) -> pure (nLen, oLen)
      -- nLen set -> derive oLen
      (Right nLen, Left mkOLen) -> mkOLen maxLenForDynCols maxOrigLen nLen
      -- derive nLen <- oLen set
      (Left mkNLen, Right oLen) -> mkNLen maxLenForDynCols maxNameLen oLen
      -- Neither set; Use both maxes if they fit, otherwise approx.
      (Left _, Left _) ->
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
      foldMap :: m (Natural, Natural) -> PathDataCore -> m (Natural, Natural)
      foldMap acc pd = do
        (!maxNameSoFar, !maxOrigSoFar) <- acc
        nameLen <- pathLen $ pd ^. #fileName
        origLen <- pathLen $ pd ^. #originalPath
        pure (max maxNameSoFar nameLen, max maxOrigSoFar origLen)

      maxStart :: (Natural, Natural)
      maxStart = (Formatting.formatFileNameLenMin, Formatting.formatOriginalPathLenMin)

      pathLen :: PathI i -> m Natural
      pathLen (MkPathI p) = do
        p' <- decodeOsToFpThrowM p
        pure $ fromIntegral $ length p'

      -- Map the name format to its strategy
      nameFmtToStrategy _ (Just (ColFormatFixed nameLen)) = Right nameLen
      nameFmtToStrategy maxNameLen (Just ColFormatMax) = Right maxNameLen
      nameFmtToStrategy _ Nothing = Left mkNameLen

      -- Map the orig format to its strategy
      origFmtToStrategy _ (Just (ColFormatFixed origLen)) = Right origLen
      origFmtToStrategy maxOrigLen (Just ColFormatMax) = Right maxOrigLen
      origFmtToStrategy _ Nothing = Left mkOrigLen

      -- Given a fixed nameLen, derive a "good" origLen
      mkOrigLen maxLenForDynCols maxOrigLen nLen =
        (nLen,) -- (nLen, derived oLen)
          <$> mkDynamicLen
            maxLenForDynCols
            Formatting.formatOriginalPathLenMin
            maxOrigLen
            nLen

      -- Given a fixed origLen, derive a "good" nameLen
      mkNameLen maxLenForDynCols maxNameLen oLen =
        (,oLen) -- (derived nLen, oLen)
          <$> mkDynamicLen
            maxLenForDynCols
            Formatting.formatFileNameLenMin
            maxNameLen
            oLen

      revSort = listCmd ^. #revSort
      sort = listCmd ^. #sort

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
  (MonadLogger f) =>
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
  if dLen + cMax <= tMax
    then -- 1. O and cMaxLen fits; use both
      pure cMax
    else do
      -- 2. MaxLen will not fit; use all remaining space to print
      -- as much as we can. As we require at least minimums, this could lead
      -- to wrapping.
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
        else -- 3. Requested origLen > available space. We are going to wrap
        -- regardless, so use it and the minimum name.
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

getMaxLen :: (MonadCatch m, MonadLogger m, MonadTerminal m) => m Natural
getMaxLen = do
  tryAny getTerminalWidth >>= \case
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
