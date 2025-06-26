{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Provides utils for file system actions.
module Test.Utils
  ( -- * File System Operations
    createFiles,
    createFilesMap,
    createFilesContents,
    createFileContents,
    createDirectories,
    createDirectory,
    clearDirectory,

    -- ** Symlinks
    Symlink (..),
    createSymlinks,
    createSymlink,
    createSymlinksTarget,
    createSymlinkTarget,

    -- * Text
    TextMatch (..),
    matches,
    unlineMatches,

    -- ** HUnit
    assertMatch,
    assertMatches,

    -- * Misc
    genPathChar,
    genPathCharIO,

    -- * Golden
    goldenDiffCustom,
  )
where

import Charon.Prelude
import Data.Char qualified as Ch
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.PathWriter (createDirectoryLink, createFileLink)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.HUnit (assertFailure)

-- | Creates empty files at the specified paths.
createFiles :: (Foldable f, Functor f, HasCallStack, MonadIO m) => f OsPath -> m ()
createFiles = createFilesMap fmap

-- | Creates empty files at the specified paths.
createFilesMap ::
  (Foldable f, HasCallStack, MonadIO m) =>
  ( (OsPath -> (OsPath, ByteString)) ->
    f OsPath ->
    f (OsPath, ByteString)
  ) ->
  f OsPath ->
  m ()
createFilesMap mapper = createFilesContents . mapper (,"")

createFilesContents ::
  (Foldable f, HasCallStack, MonadIO m) =>
  f (OsPath, ByteString) ->
  m ()
createFilesContents = traverse_ createFileContents

-- | Creates files at the specified paths.
createFileContents ::
  (HasCallStack, MonadIO m) =>
  (OsPath, ByteString) ->
  m ()
createFileContents (p, c) = liftIO $ do
  exists <- doesAnyPathExist p
  -- NOTE: [Duplicate test files]
  --
  -- We want to throw an exception when "creating" the same file twice as this
  -- is always a mistake in our test code. In particular, this silent behavior
  -- played a role in the long-running OSX integration test bug.
  --
  -- On OSX, we would generated paths p1 and p2 that were unique in the sense
  -- that they were different UTF-8 byte sequences (p1 /= p2), yet both
  -- normalized to p.
  --
  -- We would then create our files as part of our setup, though notice this
  -- means we would only create one actual file -- p -- because OSX filesystem
  -- considers p1 == p2, due to normalization.
  --
  -- We would then run through our test, trying to delete both p1 and p2. The
  -- problem was that p was deleted when we deleted p1, and there was no second
  -- file for p2. We would get an exception that "p2 does not exist" because
  -- of course it didn't.
  --
  -- The fix was to properly check for equality on the __normalized__ paths
  -- in the integration tests. However, if we had ensured that we were not
  -- overwriting files in this function, we would have had a better clue about
  -- what was going wrong.
  if exists
    then throwString $ "Path already exists: " <> show p
    else do
      writeBinaryFile p c
        `catchSync` \ex -> do
          putStrLn
            $ mconcat
              [ "[Test.Utils.createFileContents] Encountered an exception\n",
                "OsPath: '",
                show p,
                "'\n",
                "Decoded: '",
                decodeLenient p,
                "'\n",
                "Contents: '",
                bsToStr c,
                "'\n",
                "Exception: '",
                displayException ex,
                "'"
              ]
          throwM ex

-- | Creates empty files at the specified paths.
createDirectories :: (Foldable f, HasCallStack, MonadIO m) => f OsPath -> m ()
createDirectories = traverse_ createDirectory

createDirectory :: (HasCallStack, MonadIO m) => OsPath -> m ()
createDirectory = liftIO . createDirectoryIfMissing True

data Symlink
  = F OsPath
  | D OsPath

createSymlinks ::
  ( Foldable f,
    HasCallStack,
    MonadIO m
  ) =>
  f Symlink ->
  m ()
createSymlinks = traverse_ createSymlink

createSymlink ::
  ( HasCallStack,
    MonadIO m
  ) =>
  Symlink ->
  m ()
createSymlink = createSymlink' . (,Nothing)

createSymlinksTarget ::
  ( Foldable f,
    HasCallStack,
    MonadIO m
  ) =>
  f (Symlink, OsPath) ->
  m ()
createSymlinksTarget = traverse_ createSymlinkTarget

createSymlinkTarget ::
  ( HasCallStack,
    MonadIO m
  ) =>
  (Symlink, OsPath) ->
  m ()
createSymlinkTarget = createSymlink' . over' _2 Just

createSymlink' :: (HasCallStack, MonadIO m) => (Symlink, Maybe OsPath) -> m ()
createSymlink' (p, mtarget) = liftIO $ case p of
  F src -> createFileLink (fromMaybe [osp|dummy|] mtarget) src
  D src -> createDirectoryLink (fromMaybe [osp|dummy|] mtarget) src

-- | Clears a directory by deleting it if it exists and then recreating it.
clearDirectory :: (HasCallStack, MonadIO m) => OsPath -> m ()
clearDirectory path = liftIO $ do
  exists <- doesDirectoryExist path
  when exists $ removePathForcibly path
  createDirectoryIfMissing True path

-- | Data type used for testing text matches.
data TextMatch
  = Exact Text
  | Prefix Text
  | Infix Text
  | Suffix Text
  | Outfix Text Text
  | Outfixes Text [Text] Text
  deriving stock (Eq, Show)

mapTextMatch :: (Text -> Text) -> TextMatch -> TextMatch
mapTextMatch f (Exact t) = Exact (f t)
mapTextMatch f (Prefix t) = Prefix (f t)
mapTextMatch f (Infix t) = Infix (f t)
mapTextMatch f (Suffix t) = Suffix (f t)
mapTextMatch f (Outfix s e) = Outfix (f s) (f e)
mapTextMatch f (Outfixes s ins e) = Outfixes (f s) (f <$> ins) (f e)

-- | Tests text for matches via 'matches'. Otherwise triggers an HUnit failure.
assertMatches :: (MonadIO m) => [TextMatch] -> [Text] -> m ()
assertMatches expectations results = case matches expectations results of
  Nothing -> pure ()
  Just err ->
    liftIO
      $ assertFailure
      $ mconcat
        [ err,
          "\n\n*** Full expectations ***\n\n",
          unlineMatches expectations,
          "\n*** Full results ***\n\n",
          T.unpack (T.unlines results)
        ]

-- | Tests text for matches. Otherwise triggers an HUnit failure.
assertMatch :: (MonadIO m) => TextMatch -> Text -> m ()
assertMatch expectation result =
  liftIO
    $ unless (isMatchHelper expectation result)
    $ assertFailure
    $ mconcat
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
      Just
        $ mconcat
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
  start
    `T.isPrefixOf` r
    && L.all (`T.isInfixOf` r) ins
    && end
    `T.isSuffixOf` r

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
      foldl' (\acc t -> acc <> wc <> T.unpack t) "" ins,
      wc,
      T.unpack end
    ]

wc :: String
wc = "**"

-- | Generates a platform-independent char suitable for usage in a path.
genPathChar :: (MonadGen m) => Bool -> m Char
genPathChar asciiOnly = Gen.filterT filterFn (charMapper <$> genChar asciiOnly)
  where
    filterFn c = isGoodChar c && Ch.isAlphaNum c

-- | genPathChar, except prints the chars as they are generated.
genPathCharIO :: (MonadGen m, MonadIO m) => Bool -> m Char
genPathCharIO asciiOnly = do
  x <- charMapper <$> genChar asciiOnly
  liftIO
    $ putStrLn
    $ mconcat
      [ "genPathCharIO: (",
        [x],
        ", ",
        show $ Ch.isPrint x,
        ")"
      ]
  Gen.filterT filterFn (pure x)
  where
    filterFn c = isGoodChar c && Ch.isAlphaNum c

genChar :: (MonadGen m) => Bool -> m Char
isGoodChar :: Char -> Bool
badChars :: HashSet Char
charMapper :: Char -> Char

#if OSX
-- Mac + unicode is a disaster for reliability here. I am not sure where the
-- problem is, but, whatever the cause, generating arbitrary unicode and
-- encoding to _valid UTF-8_ often causes "illegal byte sequence". Why?
-- Who knows. In any case, as this is not reliable, we degrade the tests to
-- ascii. Maybe one day we can improve this.
--
-- https://en.wikipedia.org/wiki/Plane_(Unicode)
-- https://en.wikipedia.org/wiki/UTF-8#Overlong_encodings
genChar _ = Gen.ascii

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      ':',
      '~'
    ]

charMapper = Ch.toLower
#elif WINDOWS
genChar True = Gen.ascii
genChar False = Gen.unicode

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

-- https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#file-and-directory-names
badChars =
  Set.fromList
    [ '/',
      '\\',
      '<',
      '>',
      ':',
      '"',
      '|',
      '?',
      '*',
      '0',
      '.',
      ' '
    ]

-- windows paths are case-insensitive by default, so let's just take
-- lower-case paths :-(
charMapper = Ch.toLower
#else
genChar True = Gen.ascii
genChar False = Gen.unicode

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      '*',
      '~'
    ]

charMapper = id
#endif

massageTextPath :: Text -> Text
#if WINDOWS
massageTextPath = T.replace "/" "\\"
#else
massageTextPath = id
#endif

-- | We use a custom diff as this will print the actual diff to stdout,
-- whereas ordinary goldenVsFile will merely print something like
-- 'files are different'. The former is especially useful when we do not have
-- easy access to the diff files e.g. CI.
goldenDiffCustom :: TestName -> FilePath -> FilePath -> IO () -> TestTree
goldenDiffCustom x = goldenVsFileDiff x diffArgs
  where
    -- Apparently, the 'diff' program exists for windows and unix on CI. Thus
    -- the arguments ["diff", "-u" "--color=always", ref, new] also seem fine.
    -- Nevertheless, we use git as it is possibly more portable.
    diffArgs ref new =
      [ "git",
        "diff",
        "--exit-code",
        "--color=always",
        "--no-index",
        ref,
        new
      ]
