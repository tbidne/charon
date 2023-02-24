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
  ( PathData,
    PathDataFormat (FormatMultiline, FormatTabular, FormatTabularAuto),
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
    MonadLoggerNamespace m,
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
  (HasCallStack, MonadTerminal m, MonadThrow m) =>
  -- | Format to use
  PathDataFormat ->
  -- | How to sort
  Sort ->
  -- | If true, reverses the sort
  Bool ->
  -- | The index to format
  Index ->
  m Text
formatIndex style sort revSort idx = case style of
  FormatMultiline -> pure $ multiline (sortFn revSort sort) idx
  FormatTabular nameLen origLen ->
    pure $ tabular (sortFn revSort sort) nameLen origLen idx
  FormatTabularAuto -> do
    -- The name and original path columns fields are dynamic, so we attempt
    -- to size the table intelligently.
    --
    -- Basic idea:
    --
    -- 1. Get the max length name and orig. If these are within the total
    --    terminal width, use those for the column lengths.
    --
    -- 2. If not, calculate fixed column sizes within the max terminal width.
    terminalWidth <- getTerminalWidth
    -- min_width = reservedLineLen (fixed columns) + min_name (4) + min_original (8)
    --
    -- This is the minimum width the terminal needs to be for proper
    -- rendering.
    let minWidthNeeded = PathData.reservedLineLen + 4 + 8

    -- maxLen is the maximum length our dynamic columns can be.
    maxLen <-
      -- Verify: terminal_width >= min_width
      --
      -- NOTE: Obviously this subtraction is unsafe. If the terminal size
      -- is less than our min requirement (reserved + name/col) then there
      -- isn't really anything we can do anyway, so dying with an error
      -- message seems fine.
      if terminalWidth < minWidthNeeded
        then
          throwString $
            mconcat
              [ "Terminal width (",
                show terminalWidth,
                ") is less than minimum width (",
                show minWidthNeeded,
                ") for automatic tabular display.",
                " Perhaps try multiline."
              ]
        else pure $ terminalWidth - PathData.reservedLineLen

    let maxLenD = fromIntegral @_ @Double maxLen

    let (nameLen, origLen) =
          if maxName + maxOrig <= maxLen
            then -- Our found maxes are within the limits; use those
              (maxName, maxOrig)
            else -- Our found maxes are too large; in this case, instead use the
            -- actual terminal width, giving 80% to the orig paths and
            -- 20% to the names.
            --
            -- nameApprox needs to be _at least_ the required minimum

              let nameApprox = max 4 (floor $ maxLenD * 0.2)
                  origApprox = maxLen - nameApprox
               in (nameApprox, origApprox)

    pure $ tabular (sortFn revSort sort) nameLen origLen idx
  where
    -- Search the index; find the longest name and orig path
    --
    -- The (4, 8) starting points comes from the header names
    -- (name, original)
    (maxName, maxOrig) = foldl' foldMap (4, 8) (idx ^. #unIndex)
    foldMap :: (Natural, Natural) -> PathData -> (Natural, Natural)
    foldMap (!maxNameSoFar, !maxOrigSoFar) pd =
      ( max maxNameSoFar (pathLen $ pd ^. #fileName),
        max maxOrigSoFar (pathLen $ pd ^. #originalPath)
      )

    pathLen = fromIntegral . Paths.applyPathI length

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
