{-# LANGUAGE CPP #-}

-- | Provides utils for file system actions.
module Test.Utils
  ( -- * File System Operations
    createFiles,
    createFilesMap,
    createFileContents,
    createDirectories,
    clearDirectory,

    -- * Text
    TextMatch (..),
    matches,
    unlineMatches,

    -- ** HUnit
    assertMatch,
    assertMatches,

    -- * Posix/Windows compat
    massagePathI,
    massagePath,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import SafeRm.Data.Paths (PathI)
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Prelude
import Test.Tasty.HUnit (assertFailure)

-- | Creates empty files at the specified paths.
createFiles :: (Foldable f, Functor f, HasCallStack, MonadIO m) => f FilePath -> m ()
createFiles = createFilesMap fmap

-- | Creates empty files at the specified paths.
createFilesMap ::
  (Foldable f, HasCallStack, MonadIO m) =>
  ( (FilePath -> (FilePath, ByteString)) ->
    f FilePath ->
    f (FilePath, ByteString)
  ) ->
  f FilePath ->
  m ()
createFilesMap mapper = createFileContents . mapper (,"")

-- | Creates files at the specified paths.
createFileContents ::
  (Foldable f, HasCallStack, MonadIO m) =>
  f (FilePath, ByteString) ->
  m ()
createFileContents paths = liftIO $
  for_ paths $
    \(p, c) ->
      writeBinaryFile (massagePath p) c
        `catchAnyCS` \ex -> do
          putStrLn $
            mconcat
              [ "[Test.Utils.createFileContents] Exception for file '",
                p,
                "' and contents '",
                bsToStr c,
                "': ",
                displayException ex
              ]
          throwCS ex

-- | Creates empty files at the specified paths.
createDirectories :: (Foldable f, HasCallStack, MonadIO m) => f FilePath -> m ()
createDirectories paths = liftIO $
  for_ paths $
    \p -> createDirectoryIfMissing True (massagePath p)

-- | Clears a directory by deleting it if it exists and then recreating it.
clearDirectory :: (HasCallStack, MonadIO m) => FilePath -> m ()
clearDirectory path = liftIO $ do
  exists <- doesDirectoryExist path'
  when exists $ removePathForcibly path'
  createDirectoryIfMissing True path'
  where
    path' = massagePath path

-- | Data type used for testing text matches.
data TextMatch
  = Exact !Text
  | Prefix !Text
  | Infix !Text
  | Suffix !Text
  | Outfix !Text !Text
  | Outfixes !Text ![Text] !Text
  deriving stock
    ( Eq,
      Show
    )

mapTextMatch :: (Text -> Text) -> TextMatch -> TextMatch
mapTextMatch f (Exact t) = Exact (f t)
mapTextMatch f (Prefix t) = Prefix (f t)
mapTextMatch f (Infix t) = Infix (f t)
mapTextMatch f (Suffix t) = Suffix (f t)
mapTextMatch f (Outfix s e) = Outfix (f s) (f e)
mapTextMatch f (Outfixes s ins e) = Outfixes (f s) (f <$> ins) (f e)

-- | Tests text for matches via 'matches'. Otherwise triggers an HUnit failure.
--
-- This function automatically replaces backslashes with forward slashes
-- for posix/windows path compatibility. Take care that any tests do not
-- rely on having actual backslash chars (as opposed to "path separators").
assertMatches :: (MonadIO m) => [TextMatch] -> [Text] -> m ()
assertMatches expectations results = case matches expectations results of
  Nothing -> pure ()
  Just err ->
    liftIO $
      assertFailure $
        mconcat
          [ err,
            "\n\n*** Full expectations ***\n\n",
            unlineMatches expectations,
            "\n*** Full results ***\n\n",
            T.unpack (T.unlines results)
          ]

-- | Tests text for matches. Otherwise triggers an HUnit failure.
--
-- This function automatically replaces backslashes with forward slashes
-- for posix/windows path compatibility. Take care that any tests do not
-- rely on having actual backslash chars (as opposed to "path separators").
assertMatch :: (MonadIO m) => TextMatch -> Text -> m ()
assertMatch expectation result =
  liftIO $
    unless (isMatchHelper expectation result) $
      assertFailure $
        mconcat
          [ "\n\n*** Expectation ***\n\n",
            showTextMatch expectation,
            "\n\n*** Result ***\n\n",
            T.unpack result
          ]

-- | If the texts do not match, returns an error string. Otherwise
-- returns 'Nothing'.
matches :: [TextMatch] -> [Text] -> Maybe String
matches [] [] = Nothing
matches s@(_ : _) [] =
  Just $ "Empty result but non-empty expectations: " <> show s
matches [] t@(_ : _) =
  Just $ "Empty expectations but non-empty result: " <> show t
matches (e : es) (t : ts) = isMatch (e :| es) (t :| ts)

-- | If the texts do not match, returns an error string. Otherwise
-- returns 'Nothing'.
isMatch :: NonEmpty TextMatch -> NonEmpty Text -> Maybe String
isMatch (s :| es) (r :| rs) =
  if isMatchHelper s (T.strip r)
    then matches es rs
    else
      Just $
        mconcat
          [ "Expected: '",
            showTextMatch s,
            "'\nReceived: '",
            T.unpack (T.strip r),
            "'"
          ]

isMatchHelper :: TextMatch -> Text -> Bool
isMatchHelper tm = isMatchHelper' (mapTextMatch massageTextPath tm)

isMatchHelper' :: TextMatch -> Text -> Bool
isMatchHelper' (Exact e) r = e == r
isMatchHelper' (Prefix e) r = e `T.isPrefixOf` r
isMatchHelper' (Infix e) r = e `T.isInfixOf` r
isMatchHelper' (Suffix e) r = e `T.isSuffixOf` r
isMatchHelper' (Outfix e1 e2) r = e1 `T.isPrefixOf` r && e2 `T.isSuffixOf` r
isMatchHelper' (Outfixes start ins end) r =
  start `T.isPrefixOf` r
    && L.all (`T.isInfixOf` r) ins
    && end `T.isSuffixOf` r

-- | Pretty show for multiple text matches.
unlineMatches :: [TextMatch] -> String
unlineMatches [] = ""
unlineMatches (t : ts) = showTextMatch t <> "\n" <> unlineMatches ts

showTextMatch :: TextMatch -> String
showTextMatch (Exact e) = T.unpack e
showTextMatch (Prefix e) = T.unpack e <> wc
showTextMatch (Infix e) = wc <> T.unpack e <> wc
showTextMatch (Suffix e) = wc <> T.unpack e
showTextMatch (Outfix e1 e2) = T.unpack e1 <> wc <> T.unpack e2
showTextMatch (Outfixes start ins end) =
  mconcat
    [ T.unpack start,
      wc,
      foldl' (\acc t -> T.unpack t <> wc <> acc) "" ins,
      T.unpack end
    ]

wc :: String
wc = "**"

massagePathI :: PathI i -> PathI i
massagePathI = Paths.liftPathI' massagePath

massagePath :: FilePath -> FilePath
massagePath = T.unpack . massageTextPath . T.pack

massageTextPath :: Text -> Text
#if WINDOWS
massageTextPath = T.replace "/" "\\"
#else
massageTextPath = id
#endif
