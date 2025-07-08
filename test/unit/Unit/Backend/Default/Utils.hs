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
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import FileSystem.OsPath qualified as OsPath
import GHC.Real (Integral (mod))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random qualified as R
import Test.Utils qualified as TestUtils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Backend.Default.Utils"
    [ props
    ]

newtype TestIO a = MkTestIO (IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadCatch,
      MonadPosixCompat,
      MonadThrow
    )
    via IO

#if !WINDOWS
deriving newtype instance MonadPosix TestIO
#endif

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
      (MkPathI fileName) <- forAll genPath
      filePath <- liftIO $ do
        tmp <- liftIO getTemporaryDirectory
        let d = tmp </> testDirName
        createDirectoryIfMissing True d
        let path = d </> fileName
        writeBinaryFile path "file"
        pure path
      r@(MkPathI uniqueFileName, _, _) <-
        runRandIO
          $ Utils.getPathInfo trashHome (MkPathI filePath)

      annotateShow r

      fileName' <- liftIO $ OsPath.decodeThrowM fileName
      uniqueFileName' <- liftIO $ OsPath.decodeThrowM uniqueFileName

      assert $ fileName' `L.isPrefixOf` uniqueFileName'
  where
    desc = "getPathType unique fileName uses original name base"

    testDirName = [ospPathSep|charon/unit/getPathTypePreservesBaseFileName|]

newtype RandIO a = MkRandIO (IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadCatch,
      MonadPosixCompat,
      MonadThrow
    )
    via IO
  deriving (MonadLogger, MonadReader TestEnv) via TestIO

#if !WINDOWS
deriving newtype instance MonadPosix RandIO
#endif

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
  Gen.filter badString
    $ Gen.string (Range.linear 1 100) (TestUtils.genPathChar True)
  where
    badString = not . null . strip

    strip =
      T.unpack
        . T.strip
        . T.pack
