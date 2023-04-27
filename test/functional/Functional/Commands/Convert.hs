-- | Tests for convert command.
module Functional.Commands.Convert
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..))

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Convert Command"
    (backendTests args <$> cartProd @Backend)

cartProd :: (Bounded a, Enum a) => [(a, a)]
cartProd = [(x, y) | x <- g, y <- g]
  where
    g = [minBound .. maxBound]

backendTests :: IO FilePath -> (Backend, Backend) -> TestTree
backendTests args backend@(src, dest) =
  testGroup
    (Backend.backendArg src ++ " -> " ++ Backend.backendArg dest)
    [ convertsBackend backend args
    ]

convertsBackend :: (Backend, Backend) -> IO FilePath -> TestTree
convertsBackend (src, dest) args = testCase "Converts backend" $ do
  testDir <- getTestPath args testDirPath
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = withSrArgs trashDir src ("delete" : filesToDelete <> dirsToDelete)

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesExist $ mkTrashInfoPaths trashDir ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' src testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- CONVERT

  let emptyArgList = withSrArgs trashDir src ["convert", "-d", Backend.backendArg dest]
  runSafeRm emptyArgList

  -- same file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesExist $ mkTrashInfoPaths trashDir ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (convertIdxSet, convertMetadata) <- runIndexMetadata' dest testDir
  assertSetEq convertExpectedIdxSet convertIdxSet
  delExpectedMetadata @=? convertMetadata
  where
    testDirPath =
      mconcat
        [ "convertsBackend-",
          Backend.backendArg src,
          "-",
          Backend.backendArg dest
        ]
    mkTrashPath p =
      MkPathI $
        mconcat
          [ "/safe-rm/functional/convert/",
            testDirPath,
            p
          ]
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' src PathTypeFile "f1" (mkTrashPath "/f1"),
          mkPathData' src PathTypeFile "f2" (mkTrashPath "/f2"),
          mkPathData' src PathTypeFile "f3" (mkTrashPath "/f3"),
          mkPathData' src PathTypeDirectory "dir1" (mkTrashPath "/dir1"),
          mkPathData' src PathTypeDirectory "dir2" (mkTrashPath "/dir2")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    convertExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' dest PathTypeFile "f1" (mkTrashPath "/f1"),
          mkPathData' dest PathTypeFile "f2" (mkTrashPath "/f2"),
          mkPathData' dest PathTypeFile "f3" (mkTrashPath "/f3"),
          mkPathData' dest PathTypeDirectory "dir1" (mkTrashPath "/dir1"),
          mkPathData' dest PathTypeDirectory "dir2" (mkTrashPath "/dir2")
        ]

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "convert"
