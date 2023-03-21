-- | Provides utils for file system actions.
--
-- @since 0.1
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

    -- * Golden Tests
    diff,

    -- * Capturing Output
    CapturedOutput (..),
    capturedToBs,

    -- * Misc
    unsafeReplaceDir,
    txtToBuilder,
    exToBuilder,
    strToBuilder,
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as L
import Data.Text qualified as T
import SafeRm.Prelude

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createFiles :: (Foldable f, Functor f, HasCallStack) => f FilePath -> IO ()
createFiles = createFilesMap fmap

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createFilesMap ::
  (Foldable f, HasCallStack) =>
  ( (FilePath -> (FilePath, ByteString)) ->
    f FilePath ->
    f (FilePath, ByteString)
  ) ->
  f FilePath ->
  IO ()
createFilesMap mapper = createFileContents . mapper (,"")

-- | Creates files at the specified paths.
--
-- @since 0.1
createFileContents ::
  (Foldable f, HasCallStack) =>
  f (FilePath, ByteString) ->
  IO ()
createFileContents paths = for_ paths $
  \(p, c) ->
    writeBinaryFile p c
      `catchAnyCS` \ex -> do
        putStrLn $
          mconcat
            [ "[Test.Utils.createFileContents] Exception for file '",
              p,
              "' and contents '",
              Char8.unpack c,
              "': ",
              displayException ex
            ]
        throwCS ex

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createDirectories :: (Foldable f, HasCallStack) => f FilePath -> IO ()
createDirectories paths =
  for_ paths $ \p -> createDirectoryIfMissing False p

-- | Clears a directory by deleting it if it exists and then recreating it.
--
-- @since 0.1
clearDirectory :: (HasCallStack) => FilePath -> IO ()
clearDirectory path = do
  exists <- doesDirectoryExist path
  when exists $ removePathForcibly path
  createDirectoryIfMissing False path

-- | Data type used for testing text matches.
--
-- @since 0.1
data TextMatch
  = Exact !Text
  | Prefix !Text
  | Infix !Text
  | Suffix !Text
  | Outfix !Text !Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | If the texts do not match, returns an error string. Otherwise
-- returns 'Nothing'.
--
-- @since 0.1
matches :: [TextMatch] -> [Text] -> Maybe String
matches [] [] = Nothing
matches s@(_ : _) [] =
  Just $ "Empty result but non-empty expectations: " <> show s
matches [] t@(_ : _) =
  Just $ "Empty expectations but non-empty result: " <> show t
matches (e : es) (t : ts) = isMatch (e :| es) (t :| ts)

-- | If the texts do not match, returns an error string. Otherwise
-- returns 'Nothing'.
--
-- @since 0.1
isMatch :: NonEmpty TextMatch -> NonEmpty Text -> Maybe String
isMatch (s :| es) (r :| rs) =
  if isMatchHelper s (T.strip r)
    then matches es rs
    else
      Just $
        mconcat
          [ "Expected: ",
            showTextMatch s,
            "\nReceived: ",
            T.unpack r
          ]

isMatchHelper :: TextMatch -> Text -> Bool
isMatchHelper (Exact e) r = e == r
isMatchHelper (Prefix e) r = e `T.isPrefixOf` r
isMatchHelper (Infix e) r = e `T.isInfixOf` r
isMatchHelper (Suffix e) r = e `T.isSuffixOf` r
isMatchHelper (Outfix e1 e2) r = e1 `T.isPrefixOf` r && e2 `T.isSuffixOf` r

-- | Pretty show for multiple text matches.
--
-- @since 0.1
unlineMatches :: [TextMatch] -> String
unlineMatches [] = ""
unlineMatches (t : ts) = showTextMatch t <> "\n" <> unlineMatches ts

showTextMatch :: TextMatch -> String
showTextMatch (Exact e) = T.unpack e
showTextMatch (Prefix e) = T.unpack e <> wc
showTextMatch (Infix e) = wc <> T.unpack e <> wc
showTextMatch (Suffix e) = wc <> T.unpack e
showTextMatch (Outfix e1 e2) = T.unpack e1 <> wc <> T.unpack e2

wc :: String
wc = "**"

-- | Represents captured input of some kind. Different constructors are
-- to make golden tests easier to understand (i.e. included labels)
data CapturedOutput
  = MonadTerminal Builder Builder
  | Logs Builder Builder
  | Exception Builder Builder
  | DeletedPaths Builder Builder
  deriving stock (Show)

-- | Transforms a list of 'CapturedOutput' into a lazy bytestring to be used
-- with golden tests.
capturedToBs :: [CapturedOutput] -> BSL.ByteString
capturedToBs =
  Builder.toLazyByteString
    . mconcat
    . L.intersperse "\n\n"
    . foldr go []
  where
    go (MonadTerminal title bs) acc = fmt "TERMINAL " title bs acc
    go (Logs title bs) acc = fmt "LOGS " title bs acc
    go (Exception title bs) acc = fmt "EXCEPTION " title bs acc
    go (DeletedPaths title bs) acc = fmt "DELETED " title bs acc
    fmt :: Builder -> Builder -> Builder -> [Builder] -> [Builder]
    fmt cons title bs acc =
      mconcat
        [ cons,
          title,
          "\n",
          bs
        ]
        : acc

-- | HACK: Our naive golden tests require exact string quality, which is a
-- problem since the full paths are non-deterministic, depending on the
-- environment. Here are some possible remedies:
--
-- 1. Don't use golden tests, or use the function that allows us to pass a
--    custom comparator.
--    R: Golden tests make updating the output extremely convenient, we're
--       not ready to give up on an easy diff.
-- 2. Use a typeclass to mock the directory so it can be deterministic.
--    R: The main problem here is that we need a _real_ path since we are
--       interacting with the actual filesystem. We would need to somehow
--       separate the "logged path" vs. the "used path" which sounds very
--       complicated.
-- 3. Search the output text for the non-deterministic path, and replace it
--    it with a fixed substitute.
--    R. This is something of a "hack", though it is simple and easy to
--       implement.
--
-- We currently use option 3.
--
-- NOTE: We have a complication. /tmp is a likely temporary directory
-- (e.g. CI), which will call
--
--     T.replace "/tmp" "<dir>"
--
-- replacing all occurrences of /tmp, as expected. But safe-rm/tmp is an
-- actual path that is used and logged (i.e. in permanent delete), so that
-- will be replaced with "safe-rm/<dir>", which will break our tests. For
-- example, X/single.golden has the following line:
--
--    [2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs] Tmp dir: <dir>/safe-rm/tmp
--
-- Thus, if the environment happens have the temporary directory /tmp,
-- this line will be changed to <dir>/safe-rm/<dir>, hence a test failure.
--
-- The solution is to first split the string on the path we want to preserve,
-- "safe-rm/tmp". We then replace the temp dir (possibly "/tmp") on each
-- substring, before finally concatenating everything together, adding
-- "safe-rm/tmp" back.
--
-- __WARNING:__ This function is not total! It calls error for multiple
-- matches on "safe-rm/tmp". This should never happen and is definitely an
-- error.
unsafeReplaceDir :: (HasCallStack) => FilePath -> Text -> Text
unsafeReplaceDir fp txt = case T.splitOn "safe-rm/tmp" txt of
  -- Expected case 1: We have exactly one match, thus we split into two
  -- elements.
  [first, second] ->
    mconcat
      [ replaceFn first,
        "safe-rm/tmp",
        replaceFn second
      ]
  -- Expected cases 2,3: No matches
  [x] -> replaceFn x
  [] -> ""
  -- Unexpected case: We should _never_ have multiple matches for safe-rm/tmp.
  -- Technically we could write this function in a total way i.e. match on
  -- (first : second : rest) and add a 'mconcat (replaceFn <$> rest)' line.
  --
  -- But this way ensures we get a more informative error immediately.
  (_ : _ : _) ->
    error $
      mconcat
        [ "FileUtils.unsafeReplaceDir: found multiple matches for 'safe-rm/tmp' in: ",
          T.unpack txt
        ]
  where
    replaceFn = T.replace (T.pack fp) "<dir>"

-- | Diff algorithm
diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

-- | Text to ByteString Builder
txtToBuilder :: Text -> Builder
txtToBuilder = Builder.byteString . encodeUtf8

-- | Exception to ByteString Builder. If a filepath is given, replaces it.
exToBuilder :: (Exception e) => Maybe FilePath -> e -> Builder
exToBuilder Nothing = txtToBuilder . T.pack . displayException
exToBuilder (Just fp) = txtToBuilder . unsafeReplaceDir fp . T.pack . displayException

-- | String to ByteString Builder
strToBuilder :: String -> Builder
strToBuilder = txtToBuilder . T.pack
