{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides internal utility functions
module Charon.Utils
  ( -- * FAM combinators
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

    -- * Paths
    getPathType,

    -- * PathSize
    getPathSize,
    getPathSizeIgnoreDirSize,
    getSymLinkSize,

    -- * Exceptions
    displayEx,
    displayExT,
    noCallstacks,

    -- * Printing
    putLine,
    displayList,

    -- * Input
    askYesNoQ,
    getStrippedLine,

    -- * Misc
    filterSeqM,
    renderPretty,
    getAllFiles,
    localTimeToMillis,
    getRandomTmpFile,
  )
where

import Charon.Backend.Default.Exception
  ( TrashDirFilesNotFoundE,
    TrashDirInfoNotFoundE,
  )
import Charon.Exception
  ( BackendDetectE,
    DotsPathE,
    EmptyPathE,
    EmptySearchResults,
    FileNameEmptyE,
    InfoDecodeE,
    PathNotFound (MkPathNotFound),
    RenameDuplicateE,
    RestoreCollisionE,
    RootE,
    TildePathE,
    TrashEntryFileNotFoundE,
    TrashEntryInfoBadExtE,
    TrashEntryInfoNotFoundE,
    TrashEntryNotFoundE,
    TrashEntryWildcardNotFoundE,
    UniquePathNotPrefixE,
  )
import Charon.Prelude
import Control.Exception
  ( Exception (toException),
    SomeException (SomeException),
  )
import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Bytes qualified as Bytes
import Data.Bytes.Class.RawNumeric (RawNumeric (Raw))
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes.Formatting.Base (BaseFormatter)
import Data.Bytes.Size (Sized)
import Data.Foldable qualified as F
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Internal (Text (Text))
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Search qualified as TIS
import Data.Time (LocalTime, UTCTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time.Posix
import Effects.FileSystem.PathReader qualified as PR
#if WINDOWS
import Effects.System.PosixCompat.Files qualified as PXC
#else
import Effects.System.Posix.Files qualified as PX
import System.OsString.Internal.Types (OsString(OsString))
#endif
import Effects.Time (MonadTime (getMonotonicTime))
import FileSystem.OsPath (TildeException)
import FileSystem.OsPath qualified as OsPath
import PathSize
  ( PathSizeResult
      ( PathSizeFailure,
        PathSizePartial,
        PathSizeSuccess
      ),
  )
import PathSize qualified
import PathSize.Data.Config qualified as PathSize.Config
import PathSize.Utils qualified
import System.PosixCompat.Files qualified as PFiles
import Text.Printf (PrintfArg)
import URI.ByteString (Absolute)
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
  ( BaseFormatter (Raw a) ~ FloatingFormatter,
    PrintfArg (Raw a),
    Sized a,
    RawNumeric a
  ) =>
  a ->
  Text
formatBytes =
  Bytes.formatSized
    (MkFloatingFormatter (Just 2))
    Bytes.sizedFormatterUnix

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

-- | Renders via Display instance.
renderPretty :: (Display a) => a -> Text
renderPretty = display

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
percentEncode :: ByteString -> Either String ByteString
percentEncode bs =
  -- We previously used uri-bytestring's
  -- 'urlEncode :: [Word8] -> ByteString -> Builder', but the corresponding
  -- 'urlDecode' was removed, hence we must use the high-level API.
  -- We therefore:
  --
  --   1. Manually create a URIRef, specifying the scheme ("file").
  --   2. Serialize this.
  --   3. Drop the "file:" prefix from the final result, as it is not part
  --      of the xdg spec.
  --
  -- This seems to work, and as a bonus, we do not have to manually
  -- specify the 'unreserved' characters ("/-_.~"), as they are part of the
  -- high-level parsers.
  --
  -- See NOTE: [Percent decoding]
  dropFilePrefix
    . BSL.toStrict
    . Builder.toLazyByteString
    . URI.serializeURIRef
    . toUriRef
    $ bs
  where
    dropFilePrefix =
      maybe (Left err) Right . C8.stripPrefix "file:"

    err =
      mconcat
        [ "Error percent-encoding '",
          C8.unpack bs,
          "': No 'file:' prefix"
        ]

    toUriRef :: ByteString -> URI.URIRef Absolute
    toUriRef uriPath =
      URI.URI
        { URI.uriScheme = URI.Scheme "file",
          URI.uriAuthority = Nothing,
          URI.uriPath = uriPath,
          URI.uriQuery = mempty,
          URI.uriFragment = Nothing
        }

-- | Percent decodes a bytestring.
percentDecode :: ByteString -> Either String ByteString
percentDecode bs =
  -- NOTE: [Percent decoding]
  --
  -- We previously used uri-bytestring's
  -- 'urlDecode :: Bool -> ByteString -> ByteString', which was a low-level
  -- function that handled percent encoding, without any of the other stuff
  -- (e.g. schemes, query params) we did not care about.
  --
  -- However, this was removed in version 0.4, hence we are left with the
  -- higher-level API. This seems to work okay, though because it is a general
  -- URI parser, we need to specify the scheme ("file" vs. e.g. "http").
  --
  -- We do _not_ include the scheme when encoding -- since xdg does not --
  -- hence we need to manually prepend it here.
  bimap showErr URI.uriPath
    . URI.parseURI URI.strictURIParserOptions
    . ("file:" <>)
    $ bs
  where
    showErr e =
      mconcat
        [ "Error percent-decoding '",
          C8.unpack bs,
          "': ",
          show e
        ]

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

getPathSize ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
    MonadTerminal m
  ) =>
  OsPath ->
  m (Bytes B Natural)
getPathSize = getPathSizeConfig PathSize.Config.defaultConfig

getPathSizeIgnoreDirSize ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
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
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
    MonadTerminal m
  ) =>
  PathSize.Config ->
  OsPath ->
  m (Bytes B Natural)
getPathSizeConfig config path = addNamespace "getPathSizeConfig" $ do
  fmap (MkBytes @B)
    $ PathSize.pathSizeRecursiveConfig config path
    >>= \case
      PathSizeSuccess n -> pure $ fromℤ n
      PathSizePartial errs n -> do
        -- We received a value but had some errors.
        putStrLn "Encountered errors retrieving size."
        for_ errs $ \e -> do
          let errMsg = T.pack $ displayException e
          putTextLn errMsg
          $(logWarn) errMsg
        pure $ fromℤ n
      PathSizeFailure errs -> do
        putStrLn "Encountered errors retrieving size. Defaulting to 0. See logs."
        for_ errs $ \e -> do
          let errMsg = T.pack $ displayException e
          putTextLn errMsg
          $(logWarn) errMsg
        pure 0

getAllFiles ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPosixFilesC m
  ) =>
  OsPath ->
  m [OsPath]
getAllFiles fp = do
  -- see NOTE: [getPathType]
  getPathType fp >>= \case
    PathTypeSymbolicLink -> pure [fp]
    PathTypeFile -> pure [fp]
    PathTypeOther -> pure [fp]
    PathTypeDirectory ->
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
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadThrow m,
    MonadTime m
  ) =>
  OsPath ->
  m OsPath
getRandomTmpFile prefix = addNamespace "getRandomTmpFile" $ do
  -- NOTE: [File name collisions]
  --
  -- Is getMonotonicTimeNSec less likely to have collisions than
  -- getMonotonicTime? If so, consider switching. We can also add a random
  -- number if we are feeling paranoid.
  timeStr <- OsPath.encodeValidThrowM . show =<< getMonotonicTime
  tmpDir <- PR.getTemporaryDirectory

  let tmpFile = tmpDir </> prefix <> [osp|_|] <> timeStr
  $(logDebug) $ "Generated temp file: " <> decodeDisplayExT tmpFile

  pure tmpFile

getSymLinkSize ::
  ( HasCallStack,
    MonadPosixFilesC m,
    MonadThrow m
  ) =>
  OsPath ->
  m (Bytes B Natural)
getSymLinkSize =
  fmap (fromℤ . fromIntegral . PFiles.fileSize)
    . PathSize.Utils.getFileStatus

-- | Retrieves the path type. Used so we can wrap with a custom exception,
-- for the purpose of ignoring callstacks.
getPathType ::
  ( HasCallStack,
    MonadCatch m,
    MonadPosixFilesC m
  ) =>
  OsPath ->
  m PathType
getPathType p = do
#if WINDOWS
  fp <- OsPath.decodeThrowM p
  PXC.getPathType fp
    `catch` \(_ :: IOException) -> throwM $ MkPathNotFound p
#else
  PX.getPathType (coerce p)
    `catch` \(_ :: IOException) -> throwM $ MkPathNotFound p
#endif

displayEx :: (Exception e) => e -> String
displayEx ex =
  if AnnUtils.matchesException noCallstacks ex
    then case toException ex of
      SomeException innerEx -> displayException innerEx
    else displayException ex

displayExT :: (Exception e) => e -> Text
displayExT = T.pack . displayEx

-- see NOTE: [Callstacks]
noCallstacks :: [ExceptionProxy]
noCallstacks =
  [ MkExceptionProxy @BackendDetectE,
    MkExceptionProxy @DotsPathE,
    MkExceptionProxy @EmptyPathE,
    MkExceptionProxy @EmptySearchResults,
    MkExceptionProxy @FileNameEmptyE,
    MkExceptionProxy @InfoDecodeE,
    MkExceptionProxy @PathNotFound,
    MkExceptionProxy @RenameDuplicateE,
    MkExceptionProxy @RestoreCollisionE,
    MkExceptionProxy @RootE,
    MkExceptionProxy @StringException,
    -- TildeException comes from fs-utils, for reading paths at the CLI.
    MkExceptionProxy @TildeException,
    -- TildePathE defined here, for anything that slips through.
    MkExceptionProxy @TildePathE,
    MkExceptionProxy @TrashDirFilesNotFoundE,
    MkExceptionProxy @TrashDirInfoNotFoundE,
    MkExceptionProxy @TrashEntryNotFoundE,
    MkExceptionProxy @TrashEntryWildcardNotFoundE,
    MkExceptionProxy @TrashEntryFileNotFoundE,
    MkExceptionProxy @TrashEntryInfoNotFoundE,
    MkExceptionProxy @TrashEntryInfoBadExtE,
    MkExceptionProxy @UniquePathNotPrefixE
  ]

displayList :: (Foldable f) => (a -> Text) -> f a -> Text
displayList toText = F.foldMap go
  where
    go x =
      mconcat
        [ "\n - ",
          toText x
        ]

putLine :: (MonadTerminal m) => m ()
putLine = putStrLn ""

askYesNoQ ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadThrow m
  ) =>
  Text -> m Bool
askYesNoQ s =
  getInputChar msg >>= \case
    Nothing -> throwText "Received empty response."
    Just ans
      | ans == 'y' -> pure True
      | ans == 'n' -> pure False
      | otherwise ->
          throwText
            $ mconcat
              [ "Bad answer. Expected 'y' or 'n', received: ",
                T.singleton ans
              ]
  where
    msg = T.unpack $ s <> " (y/n)? "

getStrippedLine ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadThrow m
  ) =>
  Text -> m Text
getStrippedLine prompt =
  getInputLine (T.unpack prompt) >>= \case
    Nothing -> throwText "Received empty response."
    Just s -> pure $ T.strip $ T.pack s
