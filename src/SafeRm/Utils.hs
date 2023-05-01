-- | Provides internal utility functions
module SafeRm.Utils
  ( whenLeft,
    allM1,
    fromMaybeMonoid,
    formatBytes,
    normalizedFormat,
    readLogLevel,
    logLevelStrings,
    mergeAlt,
    merge,
    renderPretty,
    matchesWildcards,
    stripInfix,
    setRefIfTrue,
    breakEqBS,
    lines',
    percentEncode,
    percentDecode,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Bytes qualified as Bytes
import Data.Bytes.Class.Wrapper (Unwrapper (Unwrapped))
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes.Formatting.Base (BaseFormatter)
import Data.Bytes.Size (Sized)
import Data.Char qualified as Ch
import Data.Text qualified as T
import Data.Text.Internal (Text (Text))
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Search qualified as TIS
import SafeRm.Prelude
import Text.Printf (PrintfArg)
import URI.ByteString qualified as URI

-- | Applies the function when we have a Left.
whenLeft :: (Applicative f) => Either e a -> (e -> f ()) -> f ()
whenLeft (Right _) _ = pure ()
whenLeft (Left x) f = f x

-- | 'allM' that must have at least one 'True'.
allM1 :: (Monad m) => NonEmpty (m Bool) -> m Bool
allM1 (m :| ms) =
  m >>= \case
    True -> allM ms
    False -> pure False

-- | 'Prelude.all' lifted to monads.
allM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
allM = foldr f (pure True)
  where
    f m acc =
      m >>= \case
        True -> acc
        False -> pure False

-- | Normalizes and formats the bytes.
normalizedFormat :: Bytes B Natural -> Text
normalizedFormat =
  formatBytes
    . Bytes.normalize
    . toDouble
  where
    toDouble :: Bytes s Natural -> Bytes s Double
    toDouble = fmap fromIntegral

-- | Formats the bytes.
formatBytes ::
  ( BaseFormatter (Unwrapped a) ~ FloatingFormatter,
    PrintfArg (Unwrapped a),
    Sized a,
    Unwrapper a
  ) =>
  a ->
  Text
formatBytes =
  Bytes.formatSized
    (MkFloatingFormatter (Just 2))
    Bytes.sizedFormatterUnix

-- | 'fromMaybe' for 'Monoid'.
fromMaybeMonoid :: (Monoid a) => Maybe a -> a
fromMaybeMonoid = fromMaybe mempty

-- | Reads the 'LogLevel'.
readLogLevel :: (MonadFail m) => Text -> m (Maybe LogLevel)
readLogLevel "none" = pure Nothing
readLogLevel "error" = pure $ Just LevelError
readLogLevel "warn" = pure $ Just LevelWarn
readLogLevel "info" = pure $ Just LevelInfo
readLogLevel "debug" = pure $ Just LevelDebug
readLogLevel other =
  fail $
    mconcat
      [ "Expected log-level ",
        logLevelStrings,
        ", received: ",
        T.unpack other
      ]

-- | String description of possible log levels parsed by 'readLogLevel'.
logLevelStrings :: String
logLevelStrings = "(none|error|warn|info|debug)"

mergeAlt ::
  (Alternative f) =>
  Lens' s (f a) ->
  Lens' t (f a) ->
  s ->
  t ->
  f a
mergeAlt = merge (<|>)

merge ::
  (a -> a -> a) ->
  Lens' s a ->
  Lens' t a ->
  s ->
  t ->
  a
merge f sLens tLens s t = (s ^. sLens) `f` (t ^. tLens)

renderPretty :: (Pretty a) => a -> Text
renderPretty =
  renderStrict
    . layoutCompact
    . pretty

matchesWildcards :: Text -> Text -> Bool
matchesWildcards matchStr toMatch = case splitMatchStr matchStr of
  -- Impossible
  [] -> False
  (m : ms) -> case T.stripPrefix m toMatch of
    Nothing -> False
    Just toMatch' -> go ms toMatch'
  where
    go [] s = T.null s
    go [""] _ = True
    go (m : ms) s = case stripInfix m s of
      Nothing -> False
      Just (_, s') -> go ms s'

    -- Because the matchStr may contain literal '*'s not representing wildcards
    -- (written as "\*"), we do not want to include them in the split.
    -- Thus we first map them to null bytes (unix paths cannot contain null
    -- bytes), then add them back.
    splitMatchStr :: Text -> [Text]
    splitMatchStr =
      fmap (T.replace "\0" "*")
        . T.split (== '*')
        . T.replace "\\*" "\0"

stripInfix :: Text -> Text -> Maybe (Text, Text)
stripInfix "" t = Just ("", t)
stripInfix p@(Text _arr _off plen) t@(Text arr off len) =
  case TIS.indices p t of
    [] -> Nothing
    (x : _) -> Just (TI.text arr off x, TI.text arr (x + off + plen) (len - plen - x))

setRefIfTrue :: (MonadIORef m) => IORef Bool -> Bool -> m ()
setRefIfTrue _ False = pure ()
setRefIfTrue ref True = writeIORef ref True

-- | Breaks a bytestring on the first '='. The '=' is removed from the second
-- element.
breakEqBS :: ByteString -> (ByteString, ByteString)
breakEqBS bs = (left, right')
  where
    (left, right) = C8.break (== '=') bs
    right' = case C8.uncons right of
      Nothing -> ""
      Just (_, rest) -> rest

-- | Lines 'BS.lines', except this if we encounter consecutive newlines, only
-- the __last__ newline is used as a "break". The prior ones are considered
-- part of the preceeeding string. E.g.
--
-- @
-- ["111","","","222","333","","444"] === lines "111\n\n\n222\n333\n\n444"
-- ["111\n\n","222","333\n","444"] === lines' "111\n\n\n222\n333\n\n444"
-- @
lines' :: ByteString -> [ByteString]
lines' "" = []
lines' bs = case BS.break (== c) bs of
  (left, right) -> case BS.uncons right of
    -- 1. No more new lines, end of the string
    Nothing -> [left]
    -- 2. Found at least one newline; check to see if there are any more
    -- consecutive ones.
    Just (_newline, rest) -> case BS.span (== c) rest of
      -- 2.a. Only found a single (above _newline) line; recurse on the rest
      ("", rest') -> left : lines' rest'
      -- 2.b. Found multiple new lines; add these back then recurse on the rest
      (newlines, rest') -> left <> newlines : lines' rest'
  where
    c = fromIntegral $ Ch.ord '\n'

-- | Percent encoded a bytestring.
percentEncode :: ByteString -> ByteString
percentEncode =
  BSL.toStrict
    . Builder.toLazyByteString
    . URI.urlEncode unreserved
  where
    -- NOTE: This is the 'mark' set defined by RFC2396 and the '/' character
    -- with one modification: The mark characters !, *, ', (, ) are excluded.
    --
    -- As the successor RFC3986 notes, these characters can be dangerous
    -- to decode, thus they were in fact moved to the 'reserved' section in
    -- that RFC.
    --
    -- Moreover, KDE Plasma's trash implementation indeed encoded these chars
    -- as well.
    --
    -- Thus we do the same thing here.
    unreserved =
      ord8
        <$> [ '/',
              '-',
              '_',
              '.',
              '~'
            ]

    ord8 :: Char -> Word8
    ord8 = fromIntegral . Ch.ord

-- | Percent decodes a bytestring.
percentDecode :: ByteString -> ByteString
percentDecode = URI.urlDecode False
