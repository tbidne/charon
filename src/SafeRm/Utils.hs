{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
    noBuffering,
    getPathSize,
    getPathSizeIgnoreDirSize,
    getAllFiles,
    localTimeToMillis,
    getRandomTmpFile,
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
import Data.Time (LocalTime, UTCTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time.Posix
import Effects.FileSystem.HandleWriter qualified as HW
import Effects.FileSystem.PathReader qualified as PR
import Effects.Time (MonadTime (getMonotonicTime))
import PathSize
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
  )
import PathSize qualified
import PathSize.Data.Config qualified as PathSize.Config
import SafeRm.Prelude
import System.IO qualified as IO
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
    -- NOTE: Why stripInfix? Say we have @matchesWildcards "foo*bar" "foobazbar"@.
    -- After the first case split above, we will have @go ["bar"] "bazbar"@.
    -- The '*' is meant to match to the right as far as possible, so we need
    -- the __first__ occurrence of "bar", wherever that occurs, which is
    -- exactly what stripInfix does.
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

-- | Sets NoBuffering.
noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = HW.hSetBuffering h NoBuffering

getPathSize ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  OsPath ->
  m (Bytes B Natural)
getPathSize = getPathSizeConfig PathSize.Config.defaultConfig

getPathSizeIgnoreDirSize ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  OsPath ->
  m (Bytes B Natural)
getPathSizeIgnoreDirSize = getPathSizeConfig config
  where
    config = set' #ignoreDirIntrinsicSize True PathSize.Config.defaultConfig

getPathSizeConfig ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  PathSize.Config ->
  OsPath ->
  m (Bytes B Natural)
getPathSizeConfig config path = do
  fmap (MkBytes @B)
    $ PathSize.pathSizeRecursiveConfig config path
    >>= \case
      PathSizeSuccess n -> pure n
      PathSizePartial errs n -> do
        -- We received a value but had some errors.
        putStrLn "Encountered errors retrieving size."
        for_ errs $ \e -> do
          let errMsg = T.pack $ displayException e
          putTextLn errMsg
          $(logError) errMsg
        pure n
      PathSizeFailure errs -> do
        putStrLn "Encountered errors retrieving size. Defaulting to 0. See logs."
        for_ errs $ \e -> do
          let errMsg = T.pack $ displayException e
          putTextLn errMsg
          $(logError) errMsg
        pure 0

getAllFiles ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  m [OsPath]
getAllFiles fp =
  -- NOTE: [getPathType]
  --
  -- It would be nice to switch this from PathReader's getPathType to
  -- PosixCompact's, as the latter is faster (fewer IO calls). Alas, the latter
  -- is also much harder to mock, which we unfortunately rely on in some tests.
  --
  -- If we figure out how to mock it (or make the tests realer), we can then
  -- swap it.
  PR.getPathType fp >>= \case
    PR.PathTypeSymbolicLink -> pure [fp]
    PR.PathTypeFile -> pure [fp]
    PR.PathTypeDirectory ->
      listDirectory fp
        >>= fmap join
        . traverse (getAllFiles . (fp </>))

localTimeToMillis :: LocalTime -> Integer
localTimeToMillis = utcTimeToMillis . Time.localTimeToUTC Time.utc

utcTimeToMillis :: UTCTime -> Integer
utcTimeToMillis = (`div` 1_000) . utcTimeToMicros

utcTimeToMicros :: UTCTime -> Integer
utcTimeToMicros utc =
  Time.diffTimeToPicoseconds (realToFrac diffTime) `div` 1_000_000
  where
    diffTime = Time.diffUTCTime utc epoch

epoch :: UTCTime
epoch = Time.Posix.posixSecondsToUTCTime 0

getRandomTmpFile ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m,
    MonadTime m
  ) =>
  OsPath ->
  m OsPath
getRandomTmpFile prefix = do
  -- NOTE: [File name collisions]
  --
  -- Is getMonotonicTimeNSec less likely to have collisions than
  -- getMonotonicTime? If so, consider switching. We can also add a random
  -- number if we are feeling paranoid.
  timeStr <- encodeFpToOsThrowM . show =<< getMonotonicTime
  tmpDir <- PR.getTemporaryDirectory
  pure $ tmpDir </> prefix <> [osp|_|] <> timeStr
