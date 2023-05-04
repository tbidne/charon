{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
module SafeRm.Data.Index
  ( Index (..),
    empty,

    -- * Reading
    readIndex,

    -- * Formatting
    formatIndex,
    Sort (..),
    readSort,

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
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathData.Core qualified as PathDataCore
import SafeRm.Data.PathData.Formatting
  ( ColFormat (..),
    PathDataFormat (..),
  )
import SafeRm.Data.PathData.Formatting qualified as Formatting
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Env (HasBackend (..), HasTrashHome)
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( InfoDecodeE (MkInfoDecodeE),
    TrashEntryFileNotFoundE (MkTrashEntryFileNotFoundE),
    TrashEntryInfoBadExtE (..),
    TrashEntryInfoNotFoundE (..),
  )
import SafeRm.Prelude
import System.FilePath qualified as FP

type PathDataCore = PathDataCore.PathData

-- | Index that stores the trash data.
newtype Index = MkIndex
  { unIndex :: Seq PathData
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Index

-- | Empty index.
empty :: Index
empty = MkIndex mempty

-- | Reads the trash directory into the 'Index'. If this succeeds then
-- everything is 'well-formed' i.e. there is a bijection between trash/files
-- and trash/info.
readIndex ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  m Index
readIndex trashHome = addNamespace "readIndex" $ do
  paths <- listDirectory trashInfoDir'
  $(logDebug) ("Trash info: " <> T.pack trashInfoDir')
  $(logDebug) ("Info: " <> T.pack (show paths))
  backend <- asks getBackend

  -- TODO: Maybe this shouldn't report errors eagerly

  let seqify ::
        Path ->
        m (Seq PathData, HashSet (PathI TrashEntryFileName)) ->
        m (Seq PathData, HashSet (PathI TrashEntryFileName))
      seqify p macc = do
        let actualExt = FP.takeExtension p
            expectedExt = Env.trashInfoExtension backend

        when (actualExt /= expectedExt) $
          throwCS $
            MkTrashEntryInfoBadExtE (MkPathI p) actualExt expectedExt

        let path = trashInfoDir' </> p
        $(logDebug) ("Path: " <> T.pack path)

        contents <- readBinaryFile path
        let -- NOTE: We want the name without the suffix
            fileName = FP.dropExtension $ FP.takeFileName path
            decoded = decode (backend, MkPathI fileName) contents
        case decoded of
          Left err -> throwCS $ MkInfoDecodeE (MkPathI path) contents err
          Right pd -> do
            throwIfTrashNonExtant trashHome pd
            (accSeq, accSet) <- macc
            pure (pd :<| accSeq, HSet.insert (pd ^. #fileName) accSet)

  (indexSeq, pathSet) <- foldr seqify (pure ([], HSet.empty)) paths

  -- NOTE: Check that all files in /files exist in the index.
  allTrashPaths <- listDirectory trashPathsDir'
  $(logDebug) ("Paths: " <> T.pack (show allTrashPaths))
  for_ allTrashPaths $ \p -> do
    let pName = MkPathI $ FP.takeFileName p
    unless (pName `HSet.member` pathSet) $
      throwCS $
        MkTrashEntryInfoNotFoundE trashHome pName

  pure $ MkIndex indexSeq
  where
    MkPathI trashPathsDir' = Env.getTrashPathDir trashHome
    MkPathI trashInfoDir' = Env.getTrashInfoDir trashHome

-- | Verifies that the 'PathData'\'s @fileName@ actually exists.
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
      MkTrashEntryFileNotFoundE trashHome filePath
  where
    filePath = pd ^. #fileName

-- | How to sort the index list.
data Sort
  = -- | Sort by name.
    Name
  | -- | Sort by size.
    Size
  deriving stock (Eq, Show)

readSort :: (MonadFail m) => Text -> m Sort
readSort "name" = pure Name
readSort "size" = pure Size
readSort other = fail $ "Unrecognized sort: " <> T.unpack other

sortFn :: Bool -> Sort -> PathDataCore -> PathDataCore -> Ordering
sortFn b = \case
  Name -> rev Formatting.sortNameCreated
  Size -> rev Formatting.sortSizeName
  where
    rev
      | b = Formatting.sortReverse
      | otherwise = id

-- | Formats the 'Index' in a pretty way.
formatIndex ::
  forall m env.
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  -- | Format to use
  PathDataFormat ->
  -- | How to sort
  Sort ->
  -- | If true, reverses the sortPathData
  Bool ->
  -- | The index to format
  Index ->
  m Text
formatIndex style sort revSort idx = addNamespace "formatIndex" $ case style of
  FormatMultiline -> multiline (sortFn revSort sort) <$> indexToSeq idx
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
    --       b. If we cannot fit the maxes within the terminal width, use the
    --          remaining width according to the following ratio: 40% for
    --          fileNames, 60% for the originalPath.

    -- maxLen := maximum terminal width
    maxLen <- getMaxLen

    -- maxLenForDynCols := available combined length for our dynamic
    -- columns (fileName + originalPath)
    maxLenForDynCols <-
      if maxLen < Formatting.minTableWidth
        then
          throwString $
            mconcat
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
    (nameLen, origLen) <- case (nameFmtToStrategy nameFormat, origFmtToStrategy origFormat) of
      (Right nLen, Right oLen) -> pure (nLen, oLen)
      (Right nLen, Left mkOLen) -> mkOLen maxLenForDynCols nLen
      (Left mkNLen, Right oLen) -> mkNLen maxLenForDynCols oLen
      (Left _, Left _) ->
        if maxNameLen + maxOrigLen <= maxLenForDynCols
          then pure (maxNameLen, maxOrigLen)
          else
            let maxLenDynColsD = fromIntegral @_ @Double maxLenForDynCols
                nameApprox = max 4 (floor $ maxLenDynColsD * 0.4)
                origApprox = maxLenForDynCols - nameApprox
             in pure (nameApprox, origApprox)

    tabular (sortFn revSort sort) nameLen origLen <$> indexToSeq idx
    where
      -- Search the index; find the longest name and orig path
      (maxNameLen, maxOrigLen) = foldl' foldMap maxStart (idx ^. #unIndex)
      foldMap :: (Natural, Natural) -> PathData -> (Natural, Natural)
      foldMap (!maxNameSoFar, !maxOrigSoFar) pd =
        ( max maxNameSoFar (pathLen $ pd ^. #fileName),
          max maxOrigSoFar (pathLen $ pd ^. #originalPath)
        )
      maxStart = (Formatting.formatFileNameLenMin, Formatting.formatOriginalPathLenMin)
      pathLen = fromIntegral . Paths.applyPathI length

      -- Map the name format to its strategy
      nameFmtToStrategy (Just (ColFormatFixed nameLen)) = Right nameLen
      nameFmtToStrategy (Just ColFormatMax) = Right maxNameLen
      nameFmtToStrategy Nothing = Left mkNameLen

      -- Map the orig format to its strategy
      origFmtToStrategy (Just (ColFormatFixed origLen)) = Right origLen
      origFmtToStrategy (Just ColFormatMax) = Right maxOrigLen
      origFmtToStrategy Nothing = Left mkOrigLen

      -- Given a fixed nameLen, derive a "good" origLen
      mkOrigLen maxLenForDynCols nLen =
        if nLen + maxOrigLen <= maxLenForDynCols
          then -- 1. Requested nLen and maxOrigLen fits; use both
            pure (nLen, maxOrigLen)
          else do
            -- 2. MaxOrigLen will not fit; use all remaining space to print
            -- as much as we can. As we require at least
            -- Formatting.formatOriginalPathLenMin, this could lead to wrapping.
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
                pure (nLen, max (maxLenForDynCols - nLen) Formatting.formatOriginalPathLenMin)
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
                      showt Formatting.formatOriginalPathLenMin
                    ]
                pure (nLen, Formatting.formatOriginalPathLenMin)

      -- Given a fixed origLen, derive a "good" nameLen
      mkNameLen maxLenForDynCols oLen =
        if oLen + maxNameLen <= maxLenForDynCols
          then -- 1. Requested oLen and maxNameLen fits; use both
            pure (maxNameLen, oLen)
          else do
            -- 2. MaxNameLen will not fit; use all remaining space to print
            -- as much as we can. As we require at least
            -- Formatting.formatFileNameLenMin, this could lead to wrapping.
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
                pure (max (maxLenForDynCols - oLen) Formatting.formatFileNameLenMin, oLen)
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
                      showt Formatting.formatFileNameLenMin
                    ]
                pure (Formatting.formatFileNameLenMin, oLen)

getMaxLen :: (MonadCatch m, MonadLogger m, MonadTerminal m) => m Natural
getMaxLen = do
  tryAny getTerminalWidth >>= \case
    Right w -> pure w
    Left err -> do
      $(logWarn) $
        "Could not detect terminal length. Falling back to default 80. Error:\n"
          <> displayExceptiont err
      pure 80

multiline :: (PathDataCore -> PathDataCore -> Ordering) -> Seq PathDataCore -> Text
multiline sort =
  T.intercalate "\n\n"
    . fmap Formatting.formatMultiLine
    . toList
    . getElems sort

tabular ::
  (PathDataCore -> PathDataCore -> Ordering) ->
  Natural ->
  Natural ->
  Seq PathDataCore ->
  Text
tabular sort nameLen origLen =
  ((Formatting.formatTabularHeader nameLen origLen <> "\n") <>)
    . T.intercalate "\n"
    . fmap (Formatting.formatTabularRow nameLen origLen)
    . toList
    . getElems sort

getElems ::
  (PathDataCore -> PathDataCore -> Ordering) ->
  Seq PathDataCore ->
  Seq PathDataCore
getElems = Seq.sortBy

fromList :: [PathData] -> HashMap (PathI 'TrashEntryFileName) PathData
fromList = foldr insert HMap.empty

insert ::
  PathData ->
  HashMap (PathI 'TrashEntryFileName) PathData ->
  HashMap (PathI 'TrashEntryFileName) PathData
insert pd = HMap.insert (pd ^. #fileName) pd

indexToSeq ::
  ( HasCallStack,
    HasTrashHome env,
    MonadLogger m,
    MonadPathReader m,
    MonadPathSize m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Index ->
  m (Seq PathDataCore)
indexToSeq = traverse PathData.normalizeCore . view #unIndex
