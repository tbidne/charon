{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.Index
  ( Index (..),

    -- * Reading
    readIndex,

    -- * Formatting
    formatIndex,
    Sort (..),
    readSort,
    _Name,
    _Size,

    -- * Low level utils
    fromList,
    insert,
  )
where

import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.Ord (Ord (max))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.System.Terminal (getTerminalWidth)
import GHC.Real (RealFrac (..))
import SafeRm.Data.PathData
  ( ColFormat (..),
    PathData,
    PathDataFormat (..),
    sortCreatedName,
  )
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( InfoDecodeE (MkInfoDecodeE),
    TrashInfoNotFoundE (..),
    TrashPathNotFoundE (MkTrashPathNotFoundE),
  )
import SafeRm.Prelude
import System.FilePath qualified as FP

-- | Index that stores the trash data.
--
-- @since 0.1
newtype Index = MkIndex
  { unIndex :: Seq PathData
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
      NFData
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via (Seq PathData)

-- | @since 0.1
makeFieldLabelsNoPrefix ''Index

-- | @since 0.1
instance Pretty Index where
  pretty =
    vsep
      . fmap pretty
      . toList
      . Seq.sortBy sortCreatedName
      . view #unIndex

-- | Reads the trash directory into the 'Index'. If this succeeds then
-- everything is 'well-formed' i.e. there is a bijection between trash/paths
-- and trash/info.
--
-- @since 0.1
readIndex ::
  forall m.
  ( HasCallStack,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  m Index
readIndex trashHome = addNamespace "readIndex" $ do
  paths <- listDirectory trashInfoDir'
  $(logDebug) ("Trash info: " <> T.pack trashInfoDir')
  $(logDebug) ("Info: " <> T.pack (show paths))

  let seqify ::
        Path ->
        m (Seq PathData, HashSet (PathI TrashName)) ->
        m (Seq PathData, HashSet (PathI TrashName))
      seqify p macc = do
        let path = trashInfoDir' </> p
        $(logDebug) ("Path: " <> T.pack path)

        contents <- readBinaryFile path
        let -- NOTE: We want the name without the suffix
            fileName = FP.dropExtension $ FP.takeFileName path
            decoded = PathData.decode (MkPathI fileName) contents
        case decoded of
          Left err -> throwCS $ MkInfoDecodeE (MkPathI path) contents err
          Right pd -> do
            throwIfTrashNonExtant trashHome pd
            (accSeq, accSet) <- macc
            pure (pd :<| accSeq, HSet.insert (pd ^. #fileName) accSet)

  (indexSeq, pathSet) <- foldr seqify (pure ([], HSet.empty)) paths

  -- NOTE: Check that all paths in /paths exist in the index.
  allTrashPaths <- listDirectory trashPathsDir'
  $(logDebug) ("Paths: " <> T.pack (show allTrashPaths))
  for_ allTrashPaths $ \p -> do
    let pName = MkPathI $ FP.takeFileName p
    unless (pName `HSet.member` pathSet) $
      throwCS $
        MkTrashInfoNotFoundE trashHome pName

  pure $ MkIndex indexSeq
  where
    MkPathI trashPathsDir' = Env.getTrashPathDir trashHome
    MkPathI trashInfoDir' = Env.getTrashInfoDir trashHome

-- | Verifies that the 'PathData'\'s @fileName@ actually exists.
--
-- @since 0.1
throwIfTrashNonExtant ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m ()
throwIfTrashNonExtant trashHome pd = do
  exists <- PathData.trashPathExists trashHome pd
  unless exists $
    throwCS $
      MkTrashPathNotFoundE trashHome filePath
  where
    filePath = pd ^. #fileName

-- | How to sort the index list.
--
-- @since 0.1
data Sort
  = -- | Sort by name.
    --
    -- @since 0.1
    Name
  | -- | Sort by size.
    --
    -- @since 0.1
    Size
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''Sort

-- | @since 0.1
instance Semigroup Sort where
  l <> Name = l
  _ <> Size = Size

-- | @since 0.1
instance Monoid Sort where
  mempty = Name

-- | @since 0.1
readSort :: (MonadFail m) => Text -> m Sort
readSort "name" = pure Name
readSort "size" = pure Size
readSort other = fail $ "Unrecognized sort: " <> T.unpack other

sortFn :: Bool -> Sort -> PathData -> PathData -> Ordering
sortFn b = \case
  Name -> rev PathData.sortNameCreated
  Size -> rev PathData.sortSizeName
  where
    rev
      | b = PathData.sortReverse
      | otherwise = id

-- | Formats the 'Index' in a pretty way.
--
-- @since 0.1
formatIndex ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadLoggerNS m,
    MonadTerminal m
  ) =>
  -- | Format to use
  PathDataFormat ->
  -- | How to sort
  Sort ->
  -- | If true, reverses the sort
  Bool ->
  -- | The index to format
  Index ->
  m Text
formatIndex style sort revSort idx = addNamespace "formatIndex" $ case style of
  FormatMultiline -> pure $ multiline (sortFn revSort sort) idx
  FormatTabular nameFormat origFormat -> do
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
    --
    --       b. If we cannot fit the maxes within the terminal width, use the
    --          remaining width according to the following ratio: 20% for
    --          fileNames, 80% for the originalPath.

    -- maxLen := maximum terminal width
    maxLen <- getMaxLen

    -- maxLenForDynCols := available combined length for our dynamic
    -- columns (fileName + originalPath)
    maxLenForDynCols <-
      if maxLen < PathData.minTableWidth
        then
          throwString $
            mconcat
              [ "Terminal width (",
                show maxLen,
                ") is less than minimum width (",
                show PathData.minTableWidth,
                ") for automatic tabular display.",
                " Perhaps try multiline."
              ]
        else pure $ maxLen - PathData.reservedLineLen

    -- Basically: if an option is explicitly specified; use it. Otherwise,
    -- try to calculate a "good" value.
    --
    -- REVIEW: It would be nice to do this with less boilerplate.
    (nameLen, origLen) <- case (nameFormat, origFormat) of
      (Just (ColFormatFixed nameLen), Just (ColFormatFixed origLen)) -> pure (nameLen, origLen)
      (Just (ColFormatFixed nameLen), Just ColFormatMax) -> pure (nameLen, maxOrigLen)
      (Just (ColFormatFixed nameLen), Nothing) -> (nameLen,) <$> mkOrigLen maxLenForDynCols nameLen
      (Just ColFormatMax, Just (ColFormatFixed origLen)) -> pure (maxNameLen, origLen)
      (Just ColFormatMax, Just ColFormatMax) -> pure (maxNameLen, maxOrigLen)
      (Just ColFormatMax, Nothing) -> (maxNameLen,) <$> mkOrigLen maxLenForDynCols maxNameLen
      (Nothing, Just (ColFormatFixed origLen)) -> (,origLen) <$> mkNameLen maxLenForDynCols origLen
      (Nothing, Just ColFormatMax) -> (,maxOrigLen) <$> mkNameLen maxLenForDynCols maxOrigLen
      (Nothing, Nothing) ->
        if maxNameLen + maxOrigLen <= maxLenForDynCols
          then pure (maxNameLen, maxOrigLen)
          else
            let maxLenDynColsD = fromIntegral @_ @Double maxLenForDynCols
                nameApprox = max 4 (floor $ maxLenDynColsD * 0.3)
                origApprox = maxLenForDynCols - nameApprox
             in pure (nameApprox, origApprox)

    pure $ tabular (sortFn revSort sort) nameLen origLen idx
    where
      -- Search the index; find the longest name and orig path
      (maxNameLen, maxOrigLen) = foldl' foldMap maxStart (idx ^. #unIndex)
      foldMap :: (Natural, Natural) -> PathData -> (Natural, Natural)
      foldMap (!maxNameSoFar, !maxOrigSoFar) pd =
        ( max maxNameSoFar (pathLen $ pd ^. #fileName),
          max maxOrigSoFar (pathLen $ pd ^. #originalPath)
        )

      maxStart = (PathData.formatFileNameLenMin, PathData.formatOriginalPathLenMin)

      pathLen = fromIntegral . Paths.applyPathI length

      mkOrigLen maxLenForDynCols nLen =
        if nLen + maxOrigLen < maxLenForDynCols
          then -- 1. Requested nLen and maxOrigLen fits; use both
            pure maxOrigLen
          else do
            -- 2. MaxOrigLen will not fit; use all remaining space to print
            -- as much as we can. As we require at least
            -- PathData.formatOriginalPathLenMin, this could lead to wrapping.
            if nLen < maxLenForDynCols
              then do
                $(logDebug) $
                  mconcat
                    [ "Maximum original path (",
                      showt maxOrigLen,
                      ") does not fit with requested name length (",
                      showt nLen,
                      ") and calculated terminal space (",
                      showt maxLenForDynCols,
                      ")"
                    ]
                pure $ max (maxLenForDynCols - nLen) PathData.formatOriginalPathLenMin
              else -- 3. Requested nameLen > available space. We are going to wrap
              -- regardless, so use it and the minimum orig.
              do
                $(logWarn) $
                  mconcat
                    [ "Requested name length (",
                      showt nLen,
                      ") > calculated terminal space (",
                      showt maxLenForDynCols,
                      "). Falling back to minimum original path len: ",
                      showt PathData.formatOriginalPathLenMin
                    ]
                pure PathData.formatOriginalPathLenMin

      mkNameLen maxLenForDynCols oLen =
        if oLen + maxNameLen < maxLenForDynCols
          then -- 1. Requested oLen and maxNameLen fits; use both
            pure maxNameLen
          else do
            -- 2. MaxNameLen will not fit; use all remaining space to print
            -- as much as we can. As we require at least
            -- PathData.formatFileNameLenMin, this could lead to wrapping.
            if oLen < maxLenForDynCols
              then do
                $(logDebug) $
                  mconcat
                    [ "Maximum name (",
                      showt maxNameLen,
                      ") does not fit with requested original path length (",
                      showt oLen,
                      ") and calculated terminal space (",
                      showt maxLenForDynCols,
                      ")"
                    ]
                pure $ max (maxLenForDynCols - oLen) PathData.formatFileNameLenMin
              else -- 3. Requested origLen > available space. We are going to wrap
              -- regardless, so use it and the minimum name.
              do
                $(logWarn) $
                  mconcat
                    [ "Requested original path length (",
                      showt oLen,
                      ") > calculated terminal space (",
                      showt maxLenForDynCols,
                      "). Falling back to minimum name len: ",
                      showt PathData.formatFileNameLenMin
                    ]
                pure PathData.formatFileNameLenMin

getMaxLen :: (MonadCatch m, MonadLogger m, MonadTerminal m) => m Natural
getMaxLen = do
  tryAny getTerminalWidth >>= \case
    Right w -> pure w
    Left err -> do
      $(logWarn) $
        "Could not detect terminal length. Falling back to default 80. Error:\n"
          <> displayExceptiont err
      pure 80

multiline :: (PathData -> PathData -> Ordering) -> Index -> Text
multiline sort =
  T.intercalate "\n\n"
    . fmap PathData.formatMultiLine
    . toList
    . getElems sort

tabular ::
  (PathData -> PathData -> Ordering) ->
  Natural ->
  Natural ->
  Index ->
  Text
tabular sort nameLen origLen =
  ((PathData.formatTabularHeader nameLen origLen <> "\n") <>)
    . T.intercalate "\n"
    . fmap (PathData.formatTabularRow nameLen origLen)
    . toList
    . getElems sort

getElems :: (PathData -> PathData -> Ordering) -> Index -> Seq PathData
getElems sort =
  Seq.sortBy sort
    . view #unIndex

-- | @since 0.1
fromList :: [PathData] -> HashMap (PathI 'TrashName) PathData
fromList = foldr insert HMap.empty

-- | @since 0.1
insert ::
  PathData ->
  HashMap (PathI 'TrashName) PathData ->
  HashMap (PathI 'TrashName) PathData
insert pd = HMap.insert (pd ^. #fileName) pd
