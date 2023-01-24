{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.Index
  ( Index (..),

    -- * Reading
    readIndex,
    searchIndex,

    -- * Writing
    appendIndex,
    writeIndex,

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

import Data.ByteString qualified as BSL
import Data.ByteString.Char8 qualified as Char8
import Data.Csv (HasHeader (HasHeader))
import Data.Csv qualified as Csv
import Data.Csv.Streaming (Records (Cons, Nil))
import Data.Csv.Streaming qualified as Csv.Streaming
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Ord (Ord (max))
import Data.Text qualified as T
import Effects.System.Terminal (getTerminalWidth)
import GHC.Real (RealFrac (..))
import SafeRm.Data.PathData
  ( PathData,
    PathDataFormat (FormatMultiline, FormatTabular, FormatTabularAuto),
    sortCreatedName,
  )
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashIndex, TrashName),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Data.UniqueSeq qualified as USeq
import SafeRm.Exception
  ( DuplicateIndexPathE (MkDuplicateIndexPathE),
    ReadIndexE (MkReadIndexE),
    TrashPathNotFoundE (MkTrashPathNotFoundE),
  )
import SafeRm.Prelude
import System.FilePath qualified as FP

-- | Index that stores the trash data.
--
-- @since 0.1
newtype Index = MkIndex
  { unIndex :: HashMap (PathI TrashName) PathData
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
    via (HashMap (PathI TrashName) PathData)

-- | @since 0.1
makeFieldLabelsNoPrefix ''Index

-- | @since 0.1
instance Pretty Index where
  pretty =
    vsep
      . fmap pretty
      . L.sortBy sortCreatedName
      . HMap.elems
      . view #unIndex

-- | Attempts to read the trash index file. If successful, guarantees:
--
-- * All trash paths are unique.
-- * Every index entry corresponds to a path in the trash directory.
--
-- @since 0.1
readIndex ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNamespace m,
    MonadThrow m
  ) =>
  PathI TrashIndex ->
  m Index
readIndex indexPath = addNamespace "readIndex" $ do
  $(logDebug) ("Index path: " <> T.pack (indexPath ^. #unPathI))
  fmap MkIndex . readIndexWithFold foldVec $ indexPath
  where
    trashHome = Paths.indexToHome indexPath
    foldVec macc pd = do
      $(logDebug) ("Found: " <> showt pd)
      acc <- macc
      throwIfDuplicates indexPath acc pd
      throwIfTrashNonExtant trashHome pd
      pure $ HMap.insert fileName pd acc
      where
        fileName = pd ^. #fileName

-- | Searches the trash index for keys.
--
-- @since 0.1
searchIndex ::
  -- | The top-level trash keys to find e.g. @foo@ for @~\/.trash\/foo@.
  UniqueSeq (PathI TrashName) ->
  -- | The trash index.
  Index ->
  -- | The first (left) element is the given paths which do not exist
  -- in the Index; the second (right) element is the found trash data.
  ([PathI TrashName], UniqueSeq PathData)
searchIndex keys (MkIndex index) = foldr foldKeys mempty trashKeys
  where
    -- NOTE: drop trailing slashes to match our index's schema
    trashKeys = USeq.map (Paths.liftPathI' FP.dropTrailingPathSeparator) keys
    foldKeys ::
      PathI TrashName ->
      ([PathI TrashName], UniqueSeq PathData) ->
      ([PathI TrashName], UniqueSeq PathData)
    foldKeys trashKey acc@(exs, found) =
      case HMap.lookup trashKey index of
        Nothing -> prependBadPath trashKey acc
        Just pd -> (exs, USeq.append found pd)
    prependBadPath p = over' _1 (p :)

-- | Reads a csv index file and applies the fold function to each
-- 'PathData' encountered. The fold function allows 'IO' in case it is needed
-- to check any conditions (e.g. trash path actually exists).
--
-- @since 0.1
readIndexWithFold ::
  forall m a.
  ( HasCallStack,
    MonadFileReader m,
    MonadLoggerNamespace m,
    MonadThrow m,
    Monoid a
  ) =>
  -- | Fold function.
  (m a -> PathData -> m a) ->
  -- | Path to index file.
  PathI TrashIndex ->
  m a
readIndexWithFold foldFn indexPath@(MkPathI fp) =
  addNamespace "readIndexWithFold" $
    (readBinaryFile >=> runFold (pure mempty) . decode) fp
  where
    decode = Csv.Streaming.decode HasHeader . BSL.fromStrict
    -- NOTE: We fold over the Records manually because its Foldable instance
    -- swallows errors, whereas we would like to report any encountered
    -- immediately.
    runFold :: m a -> Records PathData -> m a
    -- Base case, we have parsed everything with no errors nor unconsumed
    -- input
    runFold macc (Nil Nothing "") = macc
    -- End of stream w/ an error.
    runFold _ (Nil (Just err) rest) = do
      $(logError) ("Error end of stream: " <> T.pack err)
      throwWithCallStack $
        MkReadIndexE
          indexPath
          ( mconcat
              [ err,
                " at \"",
                lbsToStr rest,
                "\""
              ]
          )
    -- No errors but there is unconsumed input. This is probably impossible,
    -- but just to cover all cases...
    runFold _ (Nil _ rest) = do
      $(logError) ("Unconsumed input: " <> lbsToTxt rest)
      throwWithCallStack $
        MkReadIndexE
          indexPath
          ("Unconsumed input: " <> lbsToStr rest)
    -- Encountered an error.
    runFold _ (Cons (Left err) _) = do
      $(logError) ("Error reading stream: " <> T.pack err)
      throwWithCallStack $ MkReadIndexE indexPath err
    -- Inductive case, run fold and recurse
    runFold macc (Cons (Right x) rest) = runFold (foldFn macc x) rest

    lbsToStr = Char8.unpack . BSL.toStrict
    lbsToTxt = decodeUtf8Lenient . BSL.toStrict

-- | Verifies that the 'PathData'\'s @fileName@ does not exist in the
-- hashmap.
--
-- @since 0.1
throwIfDuplicates ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  PathI TrashIndex ->
  HashMap (PathI TrashName) PathData ->
  PathData ->
  m ()
throwIfDuplicates indexPath trashMap pd =
  when (fileName `HMap.member` trashMap) $
    throwWithCallStack $
      MkDuplicateIndexPathE indexPath fileName
  where
    fileName = pd ^. #fileName

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
    throwWithCallStack $
      MkTrashPathNotFoundE trashHome filePath
  where
    filePath = pd ^. #fileName

-- | Appends the path data to the trash index. The header is not included.
--
-- @since 0.1
appendIndex :: MonadFileWriter m => PathI TrashIndex -> Index -> m ()
appendIndex (MkPathI indexPath) =
  appendBinaryFile indexPath
    . BSL.toStrict
    . Csv.encode
    . HMap.elems
    . view #unIndex

-- | Writes the path data to the trash index, overwriting the index if it
-- exists. The header is included.
--
-- @since 0.1
writeIndex :: MonadFileWriter m => PathI TrashIndex -> Index -> m ()
writeIndex (MkPathI indexPath) =
  writeBinaryFile indexPath
    . BSL.toStrict
    . Csv.encodeDefaultOrderedByName
    . HMap.elems
    . view #unIndex

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
readSort :: MonadFail m => Text -> m Sort
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

-- | @since 0.1
formatIndex ::
  (HasCallStack, MonadTerminal m, MonadThrow m) =>
  PathDataFormat ->
  Sort ->
  Bool ->
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
    (maxName, maxOrig) = HMap.foldl' foldMap (4, 8) (idx ^. #unIndex)
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
    . getElems sort

getElems :: (PathData -> PathData -> Ordering) -> Index -> [PathData]
getElems sort =
  L.sortBy sort
    . HMap.elems
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
