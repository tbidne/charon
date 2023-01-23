-- | Provides internal utility functions
--
-- @since 0.1
module SafeRm.Utils
  ( whenJust,
    concatMNonEmpty,
    prependMNonEmpty,
    allM1,
    fromMaybeMonoid,
    maybeMonoid,
    formatBytes,
    normalizedFormat,
    readLogLevel,
    logLevelStrings,
    mergeAlt,
    merge,
  )
where

import Data.Bytes qualified as Bytes
import Data.Bytes.Class.Wrapper (Unwrapper (Unwrapped))
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes.Formatting.Base (BaseFormatter)
import Data.Bytes.Size (Sized)
import Data.Text qualified as T
import SafeRm.Prelude
import Text.Printf (PrintfArg)

-- | Applies the function when we have a Just.
--
-- @since 0.1
whenJust :: Applicative f => Maybe t -> (t -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

-- | Concats the list with the non-empty.
--
-- @since 0.1
concatMNonEmpty :: [a] -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
concatMNonEmpty [] ne = ne
concatMNonEmpty (x : xs) Nothing = Just $ x :| xs
concatMNonEmpty (x : xs) (Just (y :| ys)) = Just $ x :| xs <> (y : ys)

-- | Prepends a value to the non-empty, turning it into a Just.
--
-- @since 0.1
prependMNonEmpty :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
prependMNonEmpty x Nothing = Just (x :| [])
prependMNonEmpty x (Just (y :| ys)) = Just (x :| y : ys)

-- | 'allM' that must have at least one 'True'.
--
-- @since 0.1
allM1 :: Monad m => NonEmpty (m Bool) -> m Bool
allM1 (m :| ms) =
  m >>= \case
    True -> allM ms
    False -> pure False

-- | 'Prelude.all' lifted to monads.
--
-- @since 0.1
allM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
allM = foldr f (pure True)
  where
    f m acc =
      m >>= \case
        True -> acc
        False -> pure False

-- | Normalizes and formats the bytes.
--
-- @since 0.1
normalizedFormat :: Bytes B Natural -> Text
normalizedFormat =
  formatBytes
    . Bytes.normalize
    . toDouble
  where
    toDouble :: Bytes s Natural -> Bytes s Double
    toDouble = fmap fromIntegral

-- | Formats the bytes.
--
-- @since 0.1
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
--
-- @since 0.1
fromMaybeMonoid :: Monoid a => Maybe a -> a
fromMaybeMonoid = fromMaybe mempty

-- | 'maybe' for 'Monoid'.
--
-- @since 0.1
maybeMonoid :: Monoid b => (a -> b) -> Maybe a -> b
maybeMonoid = maybe mempty

-- | Reads the 'LogLevel'.
--
-- @since 0.1
readLogLevel :: MonadFail m => Text -> m (Maybe LogLevel)
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
--
-- @since 0.1
logLevelStrings :: String
logLevelStrings = "[none|error|warn|info|debug]"

-- | @since 0.1
mergeAlt ::
  (Alternative f) =>
  Lens' s (f a) ->
  Lens' t (f a) ->
  s ->
  t ->
  f a
mergeAlt = merge (<|>)

-- | @since 0.1
merge ::
  (a -> a -> a) ->
  Lens' s a ->
  Lens' t a ->
  s ->
  t ->
  a
merge f sLens tLens s t = (s ^. sLens) `f` (t ^. tLens)
