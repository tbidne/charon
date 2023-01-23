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
  )
where

import Data.ByteString.Char8 qualified as Char8
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
      `catchWithCallStack` \(ex :: SomeException) -> do
        putStrLn $
          mconcat
            [ "[SafeRm.FileUtils.createFileContents] Exception for file '",
              p,
              "' and contents '",
              Char8.unpack c,
              "': ",
              displayCallStack ex
            ]
        throwWithCallStack ex

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
