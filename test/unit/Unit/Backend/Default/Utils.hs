{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wwarn #-}

module Unit.Backend.Default.Utils (tests) where

import Data.List qualified as L
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import Effects.FileSystem.Utils qualified as FS.Utils
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Real (Integral (mod))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm.Backend.Default.Utils qualified as Utils
import SafeRm.Data.PathType (PathTypeW (MkPathTypeW))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashEntryOriginalPath, TrashHome))
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
  result <- runTestIO $ Utils.getPathInfo trashHome slashPath
  expected @=? result
  where
    slashPath = MkPathI [osp|/some/path/|]
    expected =
      ( MkPathI [osp|path|],
        MkPathI [osp|/some/path|],
        MkPathTypeW PathTypeDirectory
      )

testDuplicate :: TestTree
testDuplicate = testCase "Renames duplicate" $ do
  result <- runTestIO $ Utils.getPathInfo trashHome slashPath
  expected @=? result
  where
    slashPath = MkPathI [osp|/a/duplicate|]
    expected =
      ( MkPathI [osp|duplicate (1)|],
        MkPathI [osp|/a/duplicate|],
        MkPathTypeW PathTypeFile
      )

newtype TestIO a = MkTestIO (IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow) via IO

instance MonadLogger TestIO where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNS TestIO where
  getNamespace = pure ""
  localNamespace _ m = m

instance MonadPathReader TestIO where
  makeAbsolute = liftIO . makeAbsolute
  doesPathExist p
    | p == [osp|/home/trash/files/duplicate|] = pure True
    | otherwise = liftIO $ doesPathExist p

  doesDirectoryExist p
    | p == [osp|/some/path|] = pure True
    | otherwise = liftIO $ doesDirectoryExist p

  doesFileExist p
    | p == [osp|/a/duplicate|] = pure True
    | otherwise = liftIO $ doesFileExist p

  pathIsSymbolicLink = liftIO . pathIsSymbolicLink

runTestIO :: TestIO a -> IO a
runTestIO (MkTestIO io) = io

trashHome :: PathI TrashHome
trashHome = MkPathI [osp|/home/trash|]

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
getR2 = even <$> getMonotonicTimeNSec

getR3 :: IO Bool
getR3 = (== 0) . (`mod` 3) <$> getMonotonicTimeNSec

genPath :: Gen (PathI TrashEntryOriginalPath)
genPath = MkPathI . FS.Utils.unsafeEncodeFpToOs <$> genString

genString :: Gen String
genString = Gen.string (Range.linear 1 100) genPathChar

-- FIXME: Need to exclude all illegal paths here, per throwIfIllegal

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
