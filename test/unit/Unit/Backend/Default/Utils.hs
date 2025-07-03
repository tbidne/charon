{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Unit.Backend.Default.Utils (tests) where

import Charon.Backend.Default.Utils qualified as Utils
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryOriginalPath, TrashHome),
  )
import Data.Char qualified as Ch
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import FileSystem.OsPath qualified as OsPath
import GHC.Real (Integral (mod))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
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
      testDuplicate,
      testTrailingWhitespace
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
    slashPath = MkPathI [ospPathSep|/some/path/|]
    expectedOrigPath = [ospPathSep|/some/path|]

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
    slashPath = MkPathI [ospPathSep|/a/duplicate|]
    expectedOrigPath = [ospPathSep|/a/duplicate|]

{- ORMOLU_DISABLE -}

testTrailingWhitespace :: TestTree
testTrailingWhitespace = testCase desc $ do
  (resultFileName, resultOrigPath, resultPathType) <-
    runTestIO $ Utils.getPathInfo trashHome testPathI

  MkPathI expected @=? resultFileName
  PathTypeFile @=? resultPathType ^. #unPathTypeW

  resultOrigPathStr <- OsPath.decodeThrowM (resultOrigPath ^. #unPathI)
  expectedOrigPathStr <- OsPath.decodeThrowM expected

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
    testOsPath = [ospPathSep|/some whitespace   |]
    testPathI = MkPathI testOsPath

#if WINDOWS
    desc = "Strips trailing whitespace (windows)"
    expected = [ospPathSep|some whitespace|]
#else
    desc = "Preserves trailing whitespace (unix)"
    expected = [ospPathSep|some whitespace   |]
#endif

{- ORMOLU_ENABLE -}
newtype TestIO a = MkTestIO (IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow) via IO

instance MonadLogger TestIO where
  monadLoggerLog _ _ _ _ = pure ()

data TestEnv = MkTestEnv

instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
  LabelOptic "namespace" k TestEnv TestEnv x y
  where
  labelOptic =
    lens
      (const "ns")
      const

instance MonadReader TestEnv TestIO where
  ask = pure MkTestEnv

  local _ m = m

instance MonadPathReader TestIO where
  makeAbsolute = liftIO . makeAbsolute
  doesPathExist path = do
    pathStr <- OsPath.decodeThrowM path
    e1Str <- OsPath.decodeThrowM expected

    pure $ e1Str `L.isSuffixOf` pathStr
    where
      expected = [ospPathSep|/home/trash/files/duplicate|]

  doesDirectoryExist path = do
    pathStr <- OsPath.decodeThrowM path
    expectedStr <- OsPath.decodeThrowM expected

    pure $ expectedStr `L.isSuffixOf` pathStr
    where
      expected = [ospPathSep|/some/path|]

  doesFileExist path = do
    pathStr <- OsPath.decodeThrowM path

    pure $ L.any (`L.isSuffixOf` pathStr) expecteds
    where
      expecteds =
        OsPath.unsafeDecode
          <$> [ [ospPathSep|/a/duplicate|],
                -- unix vs. windows (windows trims whitespace apparently)
                [ospPathSep|/some whitespace|],
                [ospPathSep|/some whitespace   |]
              ]

  pathIsSymbolicLink = liftIO . pathIsSymbolicLink

runTestIO :: TestIO a -> IO a
runTestIO (MkTestIO io) = io

trashHome :: PathI TrashHome
trashHome = MkPathI $ [ospPathSep|/home/trash|]

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
  deriving (MonadLogger, MonadReader TestEnv) via TestIO

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
  Gen.filter badString $ Gen.string (Range.linear 1 100) genPathChar
  where
    badString = not . null . strip

    strip =
      T.unpack
        . T.strip
        . T.pack

genPathChar :: Gen Char
genPathChar = Gen.filter isGoodChar Gen.ascii
  where
    isGoodChar = not . isBadChar

    isBadChar c =
      Ch.isControl c
        || Ch.isSpace c
        || (flip Set.member (Set.fromList badChars) $ c)

    badChars =
      [ '/',
        '.',
        '\\', -- windows
        ':',
        '~'
      ]
