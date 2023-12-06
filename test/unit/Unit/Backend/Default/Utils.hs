{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Unit.Backend.Default.Utils (tests) where

import Charon.Backend.Default.Utils qualified as Utils
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryOriginalPath, TrashHome),
  )
import Data.List qualified as L
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import Effects.FileSystem.Utils qualified as FS.Utils
import GHC.Real (Integral (mod))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath qualified as FP
import System.Random qualified as R
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Backend.Default.Utils"
    [ specs,
      props
    ]

specs :: TestTree
specs =
  testGroup
    "Specs"
    [ getPathInfoTests
    ]

getPathInfoTests :: TestTree
getPathInfoTests =
  testGroup
    "getPathInfo"
    [ testPathSlash,
      testDuplicate
    ]

testPathSlash :: TestTree
testPathSlash = testCase "Retrieves name from path ending in a /" $ do
  (resultFileName, resultOrigPath, resultPathType) <-
    runTestIO $ Utils.getPathInfo trashHome slashPath

  MkPathI [osp|path|] @=? resultFileName
  PathTypeDirectory @=? resultPathType ^. #unPathTypeW

  resultOrigPathStr <- decodeOsToFpThrowM (resultOrigPath ^. #unPathI)
  expectedOrigPathStr <- decodeOsToFpThrowM expectedOrigPath

  let errMsg =
        mconcat
          [ "Expected '",
            expectedOrigPathStr,
            "' to be a suffix of '",
            resultOrigPathStr,
            ","
          ]

  -- CI adds prefixes like the drive letter on window
  -- (e.g. /some/path -> D:\\some\\path), hence we need to check the weaker
  -- suffix check rather than equality.
  assertBool errMsg $ expectedOrigPathStr `L.isSuffixOf` resultOrigPathStr
  where
    slashPath = MkPathI $ pathSeparator </> [osp|some|] </> [osp|path|] <> pathSeparator
    expectedOrigPath = pathSeparator </> [osp|some|] </> [osp|path|]

testDuplicate :: TestTree
testDuplicate = testCase "Renames duplicate" $ do
  (resultFileName, resultOrigPath, resultPathType) <-
    runTestIO $ Utils.getPathInfo trashHome slashPath

  MkPathI [osp|duplicate (1)|] @=? resultFileName
  PathTypeFile @=? resultPathType ^. #unPathTypeW

  resultOrigPathStr <- decodeOsToFpThrowM (resultOrigPath ^. #unPathI)
  expectedOrigPathStr <- decodeOsToFpThrowM expectedOrigPath

  let errMsg =
        mconcat
          [ "Expected '",
            expectedOrigPathStr,
            "' to be a suffix of '",
            resultOrigPathStr,
            ","
          ]

  assertBool errMsg $ expectedOrigPathStr `L.isSuffixOf` resultOrigPathStr
  where
    slashPath = MkPathI $ pathSeparator </> [osp|a|] </> [osp|duplicate|]
    expectedOrigPath = pathSeparator </> [osp|a|] </> [osp|duplicate|]

newtype TestIO a = MkTestIO (IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow) via IO

instance MonadLogger TestIO where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNS TestIO where
  getNamespace = pure ""
  localNamespace _ m = m

instance MonadPathReader TestIO where
  makeAbsolute = liftIO . makeAbsolute
  doesPathExist path = do
    pathStr <- decodeOsToFpThrowM path
    e1Str <- decodeOsToFpThrowM expected

    pure $ e1Str `L.isSuffixOf` pathStr
    where
      expected =
        pathSeparator
          </> [osp|home|]
          </> [osp|trash|]
          </> [osp|files|]
          </> [osp|duplicate|]

  doesDirectoryExist path = do
    pathStr <- decodeOsToFpThrowM path
    expectedStr <- decodeOsToFpThrowM expected

    pure $ expectedStr `L.isSuffixOf` pathStr
    where
      expected =
        pathSeparator
          </> [osp|some|]
          </> [osp|path|]

  doesFileExist path = do
    pathStr <- decodeOsToFpThrowM path
    expectedStr <- decodeOsToFpThrowM expected

    pure $ expectedStr `L.isSuffixOf` pathStr
    where
      expected =
        pathSeparator
          </> [osp|a|]
          </> [osp|duplicate|]

  pathIsSymbolicLink = liftIO . pathIsSymbolicLink

pathSeparator :: OsPath
pathSeparator = FS.Utils.unsafeEncodeFpToValidOs [FP.pathSeparator]

runTestIO :: TestIO a -> IO a
runTestIO (MkTestIO io) = io

trashHome :: PathI TrashHome
trashHome = MkPathI $ pathSeparator </> [osp|home|] </> [osp|trash|]

props :: TestTree
props =
  testGroup
    "Props"
    [ getPathTypePreservesBaseFileName
    ]

getPathTypePreservesBaseFileName :: TestTree
getPathTypePreservesBaseFileName =
  testPropertyNamed desc "getPathTypePreservesBaseFileName" $ do
    property $ do
      p@(MkPathI fileName) <- forAll genPath
      r@(MkPathI uniqueFileName, _, _) <- runRandIO $ Utils.getPathInfo trashHome p

      annotateShow r

      fileName' <- liftIO $ FS.Utils.decodeOsToFpThrowM fileName
      uniqueFileName' <- liftIO $ FS.Utils.decodeOsToFpThrowM uniqueFileName

      assert $ fileName' `L.isPrefixOf` uniqueFileName'
  where
    desc = "getPathType unique fileName uses original name base"

newtype RandIO a = MkRandIO (IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow) via IO
  deriving (MonadLogger, MonadLoggerNS) via TestIO

runRandIO :: (MonadIO m, MonadCatch m, MonadTest m) => RandIO a -> m a
runRandIO (MkRandIO io) = do
  liftIO io `catchAny` \ex -> do
    annotate $ displayException ex
    failure

instance MonadPathReader RandIO where
  makeAbsolute = liftIO . makeAbsolute

  pathIsSymbolicLink _ = liftIO getR3
  doesDirectoryExist _ = liftIO getR3
  doesFileExist _ = pure True

  doesPathExist _ = liftIO getR2

getR2 :: IO Bool
getR2 = do
  x <- R.randomRIO @Int (1, 2)
  pure $ even x

getR3 :: IO Bool
getR3 = do
  x <- R.randomRIO @Int (1, 3)
  pure $ x == 0 `mod` 3

genPath :: Gen (PathI TrashEntryOriginalPath)
genPath = MkPathI . FS.Utils.unsafeEncodeFpToOs <$> genString

genString :: Gen String
genString = Gen.string (Range.linear 1 100) genPathChar

genPathChar :: Gen Char
genPathChar = Gen.filter goodChar Gen.unicode
  where
    goodChar c =
      c
        /= '\NUL'
        && c
        /= '/'
        && c
        /= '.'
