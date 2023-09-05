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

    -- * Misc
    genPathChar,
    genPathCharIO,
  )
where

import Data.Char qualified as Ch
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Text qualified as T
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.FileSystem.Utils qualified as FsUtils
import Effectful.Terminal.Static qualified as Term
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import SafeRm.Prelude
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
createFilesMap mapper = createFileContents . mapper (,"")

-- | Creates files at the specified paths.
createFileContents ::
  (Foldable f, HasCallStack, MonadIO m) =>
  f (OsPath, ByteString) ->
  m ()
createFileContents paths = liftIO
  $ runEffIO
  $ for_ paths
  $ \(p, c) ->
    FW.writeBinaryFile p c
      `catchAny` \ex -> do
        Term.putStrLn
          $ mconcat
            [ "[Test.Utils.createFileContents] Encountered an exception\n",
              "OsPath: '",
              show p,
              "'\n",
              "Decoded: '",
              FsUtils.decodeOsToFpShow p,
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
createDirectories :: (Foldable f, MonadIO m) => f OsPath -> m ()
createDirectories paths = liftIO
  $ runEffIO
  $ for_ paths
  $ \p -> PW.createDirectoryIfMissing True p

-- | Clears a directory by deleting it if it exists and then recreating it.
clearDirectory :: (MonadIO m) => OsPath -> m ()
clearDirectory path = liftIO $ runEffIO $ do
  exists <- PR.doesDirectoryExist path
  when exists $ PW.removePathForcibly path
  PW.createDirectoryIfMissing True path

-- | Data type used for testing text matches.
data TextMatch
  = Exact !Text
  | Prefix !Text
  | Infix !Text
  | Suffix !Text
  | Outfix !Text !Text
  | Outfixes !Text ![Text] !Text
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
      foldl' (\acc t -> T.unpack t <> wc <> acc) "" ins,
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
    $ runEffIO
    $ Term.putStrLn
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
-- This is hedgehog's built-in Gen.unicode restricted to plane 0
-- (Basic Multilingual Plane). For reasons I do not understand, osx on CI
-- often chokes on code points outside of this range even when properly
-- encoded as UTF-8.
--
-- This seems to be backed up by osx's behavior e.g. if you try to create a
-- a file with the filename "0x F0 B1 8D 90", you will receive the error
-- 'illegal byte sequence'. This is the UTF-8 encoding for the
-- '\201552' <-> 0x31350 code point, for the record.
--
-- Curiously, many of the 'illegal byte sequences' contain 0xF0 as the lead
-- byte in some pair e.g. 0xF0C2. This is by no means exhaustive, however.
-- The notion of "overlong sequences" seems possibly relevant, though if the
-- sequence is in fact illegal, then:
--
--    1. Why is it produced by OsPath's encodeToUtf and Text's encodeUtf8?
--    2. Why is it only osx that struggles and not linux?
--
-- In any case, we restrict osx to AlphaNum Plane 0, as these seem fine.
--
-- https://en.wikipedia.org/wiki/Plane_(Unicode)
-- https://en.wikipedia.org/wiki/UTF-8#Overlong_encodings
genChar True = Gen.ascii
genChar False =
  let
    -- s1 + s2 := Plane 0
    s1 =
      (55296, Gen.enum '\0' '\55295')
    s2 =
      (8190, Gen.enum '\57344' '\65533')
  in
    Gen.frequency [s1, s2]

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      ':'
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
      '*'
    ]

charMapper = id
#endif

massageTextPath :: Text -> Text
#if WINDOWS
massageTextPath = T.replace "/" "\\"
#else
massageTextPath = id
#endif

runEffIO ::
  Eff
    [ PW.PathWriterStatic,
      PR.PathReaderStatic,
      Term.TerminalStatic,
      FW.FileWriterStatic,
      IOE
    ]
    a ->
  IO a
runEffIO =
  runEff
    . FW.runFileWriterStaticIO
    . Term.runTerminalStaticIO
    . PR.runPathReaderStaticIO
    . PW.runPathWriterStaticIO
