{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for functional test suite.
--
-- @since 0.1
module Functional.Prelude
  ( module X,

    -- * Running SafeRm

    -- ** Runners
    FuncEnv.runSafeRm,
    FuncEnv.runSafeRmException,
    FuncEnv.runIndexMetadata,

    -- *** Data capture
    FuncEnv.captureSafeRm,
    FuncEnv.captureSafeRmLogs,
    FuncEnv.captureSafeRmExceptionLogs,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
    assertDirectoriesExist,
    assertDirectoriesDoNotExist,
    assertSetEq,

    -- * Misc
    createTestDir,
    FuncEnv.mkPathData,
    mkAllTrashPaths,
    mkTrashPaths,
    mkTrashInfoPaths,
  )
where

import Data.HashSet qualified as HSet
import Data.List ((++))
import Data.Text.Lazy qualified as TL
import Functional.Prelude.FuncEnv qualified as FuncEnv
import GHC.Exts (IsList (Item, fromList, toList))
import Numeric.Literal.Integer as X (FromInteger (afromInteger))
import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Utils as X
import Text.Pretty.Simple qualified as Pretty

-- | Asserts that files exist.
assertFilesExist :: [FilePath] -> IO ()
assertFilesExist paths =
  for_ paths $ \p -> do
    exists <- doesFileExist p
    assertBool ("Expected file to exist: " <> p) exists

-- | Asserts that files do not exist.
assertFilesDoNotExist :: [FilePath] -> IO ()
assertFilesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- doesFileExist p
    assertBool ("Expected file not to exist: " <> p) (not exists)

-- | Asserts that directories exist.
assertDirectoriesExist :: [FilePath] -> IO ()
assertDirectoriesExist paths =
  for_ paths $ \p -> do
    exists <- doesDirectoryExist p
    assertBool ("Expected directory to exist: " <> p) exists

-- | Asserts that directories do not exist.
assertDirectoriesDoNotExist :: [FilePath] -> IO ()
assertDirectoriesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- doesDirectoryExist p
    assertBool ("Expected directory not to exist: " <> p) (not exists)

mkAllTrashPaths ::
  ( Functor f,
    IsList (f FilePath),
    Item (f FilePath) ~ FilePath
  ) =>
  FilePath ->
  f FilePath ->
  f FilePath
mkAllTrashPaths trashHome paths =
  fromList (toList trashPaths ++ toList trashInfoPaths)
  where
    trashPaths = mkTrashPaths trashHome paths
    trashInfoPaths = mkTrashInfoPaths trashHome paths

mkTrashInfoPaths ::
  ( Functor f
  ) =>
  FilePath ->
  f FilePath ->
  f FilePath
mkTrashInfoPaths trashHome = fmap mkTrashInfoPath
  where
    mkTrashInfoPath p = trashHome </> "info" </> p <> ".json"

mkTrashPaths ::
  ( Functor f
  ) =>
  FilePath ->
  f FilePath ->
  f FilePath
mkTrashPaths trashHome = fmap mkTrashPath
  where
    mkTrashPath p = trashHome </> "paths" </> p

assertSetEq :: (Hashable a, Show a) => HashSet a -> HashSet a -> IO ()
assertSetEq x y = do
  unless (HSet.null xdiff) $
    assertFailure $
      TL.unpack (prettySet "Expected" "Results" xdiff y)

  unless (HSet.null ydiff) $
    assertFailure $
      TL.unpack (prettySet "Results" "Expected" ydiff x)
  where
    xdiff = HSet.difference x y
    ydiff = HSet.difference y x

    prettySet d e s t =
      mconcat
        [ d,
          " contained elements not found in ",
          e,
          ":\n",
          p' s,
          "\n",
          e,
          ":\n",
          p' t
        ]

    p' = HSet.foldl' (\acc z -> Pretty.pShow z <> "\n" <> acc) ""

-- | @createTestDir tmpDirIO moduleDir testDir@, returns
-- @tmpDir/moduleDir/testDir@, creating the moduleDir if it does not exist.
--
-- E.g.
--
-- @createTestDir args "d" "someDeleteTest" @
createTestDir :: IO FilePath -> FilePath -> FilePath -> IO FilePath
createTestDir args modDir testDir = do
  -- See Note [OSX temp symlink]
  root <- canonicalizePath =<< args
  let rootMod = root </> modDir
  createDirectoryIfMissing False rootMod
  pure $ rootMod </> testDir
