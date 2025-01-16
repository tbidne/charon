{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Unit.Backend.Default.Utils (tests) where

import Charon.Backend.Default.Utils qualified as Utils
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryOriginalPath, TrashHome),
  )
import Data.HashSet qualified as Set
import Data.List qualified as L
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import FileSystem.OsPath qualified as OsPath
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

  resultOrigPathStr <- OsPath.decodeThrowM (resultOrigPath ^. #unPathI)
  expectedOrigPathStr <- OsPath.decodeThrowM expectedOrigPath

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

  resultOrigPathStr <- OsPath.decodeThrowM (resultOrigPath ^. #unPathI)
  expectedOrigPathStr <- OsPath.decodeThrowM expectedOrigPath

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
    pathStr <- OsPath.decodeThrowM path
    e1Str <- OsPath.decodeThrowM expected

    pure $ e1Str `L.isSuffixOf` pathStr
    where
      expected =
        pathSeparator
          </> [osp|home|]
          </> [osp|trash|]
          </> [osp|files|]
          </> [osp|duplicate|]

  doesDirectoryExist path = do
    pathStr <- OsPath.decodeThrowM path
    expectedStr <- OsPath.decodeThrowM expected

    pure $ expectedStr `L.isSuffixOf` pathStr
    where
      expected =
        pathSeparator
          </> [osp|some|]
          </> [osp|path|]

  doesFileExist path = do
    pathStr <- OsPath.decodeThrowM path
    expectedStr <- OsPath.decodeThrowM expected

    pure $ expectedStr `L.isSuffixOf` pathStr
    where
      expected =
        pathSeparator
          </> [osp|a|]
          </> [osp|duplicate|]

  pathIsSymbolicLink = liftIO . pathIsSymbolicLink

pathSeparator :: OsPath
pathSeparator = OsPath.unsafeEncodeValid [FP.pathSeparator]

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

      fileName' <- liftIO $ OsPath.decodeThrowM fileName
      uniqueFileName' <- liftIO $ OsPath.decodeThrowM uniqueFileName

      assert $ fileName' `L.isPrefixOf` uniqueFileName'
  where
    desc = "getPathType unique fileName uses original name base"

newtype RandIO a = MkRandIO (IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow) via IO
  deriving (MonadLogger, MonadLoggerNS) via TestIO

runRandIO :: (MonadIO m, MonadCatch m, MonadTest m) => RandIO a -> m a
runRandIO (MkRandIO io) = do
  liftIO io `catchSync` \ex -> do
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

genPath :: (HasCallStack) => Gen (PathI TrashEntryOriginalPath)
genPath = MkPathI . OsPath.unsafeEncode <$> genString

genString :: Gen String
genString =
  Gen.filter (not . null) $ Gen.string (Range.linear 1 100) genPathChar

genPathChar :: Gen Char
genPathChar = Gen.filter isGoodChar Gen.ascii
  where
    isGoodChar = not . flip Set.member (Set.fromList badChars)
    badChars =
      [ '\NUL',
        '\SOH',
        '/',
        '.',
        '\\' -- windows
      ]
