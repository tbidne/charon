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

    -- NOTE: We want to format the table such that we (concisely) display as
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
      (Right nLen, Right oLen) -> pure (nLen, oLen)
      (Right nLen, Left mkOLen) -> mkOLen maxLenForDynCols maxOrigLen nLen
      (Left mkNLen, Right oLen) -> mkNLen maxLenForDynCols maxNameLen oLen
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
        if nLen + maxOrigLen <= maxLenForDynCols
          then -- 1. Requested nLen and maxOrigLen fits; use both
            pure (nLen, maxOrigLen)
          else do
            -- 2. MaxOrigLen will not fit; use all remaining space to print
            -- as much as we can. As we require at least
            -- Formatting.formatOriginalPathLenMin, this could lead to wrapping.
            if nLen < maxLenForDynCols
              then do
                $(logDebug)
                  $ mconcat
                    [ "Maximum original path (",
                      showt maxOrigLen,
                      ") does not fit with requested name length (",
                      showt nLen,
                      ") and calculated terminal space (",
                      showt maxLenForDynCols,
                      ")"
                    ]
                pure (nLen, max (maxLenForDynCols - nLen) Formatting.formatOriginalPathLenMin)
              else -- 3. Requested nameLen > available space. We are going to wrap
              -- regardless, so use it and the minimum orig.
              do
                $(logWarn)
                  $ mconcat
                    [ "Requested name length (",
                      showt nLen,
                      ") > calculated terminal space (",
                      showt maxLenForDynCols,
                      "). Falling back to minimum original path len: ",
                      showt Formatting.formatOriginalPathLenMin
                    ]
                pure (nLen, Formatting.formatOriginalPathLenMin)

      -- Given a fixed origLen, derive a "good" nameLen
      mkNameLen maxLenForDynCols maxNameLen oLen =
        if oLen + maxNameLen <= maxLenForDynCols
          then -- 1. Requested oLen and maxNameLen fits; use both
            pure (maxNameLen, oLen)
          else do
            -- 2. MaxNameLen will not fit; use all remaining space to print
            -- as much as we can. As we require at least
            -- Formatting.formatFileNameLenMin, this could lead to wrapping.
            if oLen < maxLenForDynCols
              then do
                $(logDebug)
                  $ mconcat
                    [ "Maximum name (",
                      showt maxNameLen,
                      ") does not fit with requested original path length (",
                      showt oLen,
                      ") and calculated terminal space (",
                      showt maxLenForDynCols,
                      ")"
                    ]
                pure (max (maxLenForDynCols - oLen) Formatting.formatFileNameLenMin, oLen)
              else -- 3. Requested origLen > available space. We are going to wrap
              -- regardless, so use it and the minimum name.
              do
                $(logWarn)
                  $ mconcat
                    [ "Requested original path length (",
                      showt oLen,
                      ") > calculated terminal space (",
                      showt maxLenForDynCols,
                      "). Falling back to minimum name len: ",
                      showt Formatting.formatFileNameLenMin
                    ]
                pure (Formatting.formatFileNameLenMin, oLen)

      revSort = listCmd ^. #revSort
      sort = listCmd ^. #sort

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
