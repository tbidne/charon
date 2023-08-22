-- | Provides internal utility functions
module SafeRm.Utils
  ( -- * FAM combinators
    throwIfEx,
    whenM,

    -- * Text
    matchesWildcards,
    stripInfix,

    -- * ByteString
    breakEqBS,

    -- ** Percent encoding
    percentEncode,
    percentDecode,

    -- * Optics
    mergeAlt,
    merge,

    -- * Bytes formatting
    normalizedFormat,
    formatBytes,

    -- * Logs
    readLogLevel,
    logLevelStrings,

    -- * Misc
    filterSeqM,
    renderPretty,
    setRefIfJust,
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Bytes qualified as Bytes
import Data.Bytes.Class.Wrapper (Unwrapper (Unwrapped))
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes.Formatting.Base (BaseFormatter)
import Data.Bytes.Size (Sized)
import Data.Char qualified as Ch
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Internal (Text (Text))
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Search qualified as TIS
import SafeRm.Prelude
import Text.Printf (PrintfArg)
import URI.ByteString qualified as URI

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

-- | Reads the 'LogLevel'.
readLogLevel :: (MonadFail m) => Text -> m (Maybe LogLevel)
readLogLevel "none" = pure Nothing
readLogLevel "error" = pure $ Just LevelError
readLogLevel "warn" = pure $ Just LevelWarn
readLogLevel "info" = pure $ Just LevelInfo
readLogLevel "debug" = pure $ Just LevelDebug
readLogLevel other =
  fail
    $ mconcat
      [ "Expected log-level ",
        logLevelStrings,
        ", received: ",
        T.unpack other
      ]

-- | String description of possible log levels parsed by 'readLogLevel'.
logLevelStrings :: String
logLevelStrings = "(none|error|warn|info|debug)"

-- | Merge two fields using the 'Alternative' instance.
mergeAlt ::
  (Alternative f) =>
  Lens' s (f a) ->
  Lens' t (f a) ->
  s ->
  t ->
  f a
mergeAlt = merge (<|>)

-- | Merge two fields using the given function.
merge ::
  (a -> a -> a) ->
  Lens' s a ->
  Lens' t a ->
  s ->
  t ->
  a
merge f sLens tLens s t = (s ^. sLens) `f` (t ^. tLens)

-- | Renders via pretty instance.
renderPretty :: (Pretty a) => a -> Text
renderPretty =
  renderStrict
    . layoutCompact
    . pretty

-- | @matchesWildcards matchStr toMatch@ returns true if @toMatch@ "matches"
-- the @matchStr@, where unescaped asterisks in @matchStr@ are interpreted
-- as wildcards.
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

-- | @stripInfix text needle@ strips the _first_ occurrence of needle
-- from the text.
stripInfix :: Text -> Text -> Maybe (Text, Text)
stripInfix "" t = Just ("", t)
stripInfix p@(Text _arr _off plen) t@(Text arr off len) =
  case TIS.indices p t of
    [] -> Nothing
    (x : _) -> Just (TI.text arr off x, TI.text arr (x + off + plen) (len - plen - x))

-- | Sets the ioref if the maybe is non-empty.
setRefIfJust :: (MonadIORef m) => IORef (Maybe a) -> Maybe a -> m ()
setRefIfJust _ Nothing = pure ()
setRefIfJust ref x@(Just _) = writeIORef ref x

-- | Throws the exception if it exists in the ref.
throwIfEx ::
  ( MonadIORef m,
    MonadThrow m
  ) =>
  IORef (Maybe SomeException) ->
  m ()
throwIfEx ref =
  readIORef ref >>= \case
    Nothing -> pure ()
    Just ex -> throwCS ex

-- | Breaks a bytestring on the first '='. The '=' is removed from the second
-- element.
breakEqBS :: ByteString -> (ByteString, ByteString)
breakEqBS bs = (left, right')
  where
    (left, right) = C8.break (== '=') bs
    right' = case C8.uncons right of
      Nothing -> ""
      Just (_, rest) -> rest

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

-- | Filter a sequence monadically.
filterSeqM :: forall m a. (Monad m) => (a -> m Bool) -> Seq a -> m (Seq a)
filterSeqM p = foldl' foldP (pure Seq.empty)
  where
    foldP :: m (Seq a) -> a -> m (Seq a)
    foldP acc x = do
      b <- p x
      if b
        then (:|> x) <$> acc
        else acc

-- | When for monadic bool.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb ma = mb >>= (`when` ma)
