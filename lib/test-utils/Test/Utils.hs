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

    -- ** HUnit
    assertMatch,
    assertMatches,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.List qualified as L
import Data.Text qualified as T
import SafeRm.Prelude
import Test.Tasty.HUnit (assertFailure)

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
  | Outfixes !Text ![Text] !Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Tests text for matches via 'matches'. Otherwise triggers an HUnit failure.
--
-- @since 0.1
assertMatches :: [TextMatch] -> [Text] -> IO ()
assertMatches expectations results = case matches expectations results of
  Nothing -> pure ()
  Just err ->
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
-- @since 0.1
assertMatch :: TextMatch -> Text -> IO ()
assertMatch expectation result =
  unless (isMatchHelper expectation result) $
    assertFailure $
      mconcat
        [ "\n\n*** Expectation ***\n\n",
          showTextMatch expectation,
          "\n*** Result ***\n\n",
          T.unpack result
        ]

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
isMatchHelper (Outfixes start ins end) r =
  start `T.isPrefixOf` r
    && L.all (`T.isInfixOf` r) ins
    && end `T.isSuffixOf` r

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
showTextMatch (Outfixes start ins end) =
  mconcat
    [ T.unpack start,
      wc,
      foldl' (\acc t -> T.unpack t <> wc <> acc) "" ins,
      T.unpack end
    ]

wc :: String
wc = "**"
