-- | Provides utils for file system actions.
--
-- @since 0.1
module SafeRm.FileUtils
  ( -- * File System Operations
    createFiles,
    createFilesMap,
    createFileContents,
    createDirectories,
    clearDirectory,

    -- * Golden Tests
    diff,

    -- * Capturing Output
    CapturedOutput (..),
    capturedToBs,

    -- * Misc
    replaceDir,
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
            [ "[SafeRm.FileUtils.createFileContents] Exception for file '",
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
clearDirectory :: HasCallStack => FilePath -> IO ()
clearDirectory path = do
  exists <- doesDirectoryExist path
  when exists $ removePathForcibly path
  createDirectoryIfMissing False path

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
replaceDir :: FilePath -> Text -> Text
replaceDir fp = T.replace (T.pack fp) "<dir>"

-- | Diff algorithm
diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

-- | Text to ByteString Builder
txtToBuilder :: Text -> Builder
txtToBuilder = Builder.byteString . encodeUtf8

-- | Exception to ByteString Builder. If a filepath is given, replaces it.
exToBuilder :: Exception e => Maybe FilePath -> e -> Builder
exToBuilder Nothing = txtToBuilder . T.pack . displayException
exToBuilder (Just fp) = txtToBuilder . replaceDir fp . T.pack . displayException

-- | String to ByteString Builder
strToBuilder :: String -> Builder
strToBuilder = txtToBuilder . T.pack
