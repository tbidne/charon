{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for integration test suite.
module Integration.Utils
  ( -- * Types
    GPaths,
    PathWithType,
    PathIntData (..),
    NormedFp (..),
    fpToNormedFp,
    normedFpToFp,

    -- * Gen
    genFileNameSet,
    gen2FileNameSets,
    gen3FileNameSets,
  )
where

import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Data.Text qualified as T
import Data.Text.Normalize (NormalizationMode (NFD))
import Data.Text.Normalize qualified as TNormalize
import Effects.FileSystem.Utils qualified as FsUtils
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import Integration.Prelude
import Test.Utils qualified as TestUtils

-- NOTE: [Unicode normalization]
--
-- It turns out, OSX normalizes paths per utf-8 standards.
--
--     https://stackoverflow.com/questions/38484369/why-does-the-c-runtime-on-mac-os-allow-both-precomposed-and-decomposed-utf-8
--
-- To see why this matters, consider the two __distinct__ unicode chars:
--
-- ώ (8061)
-- ώ (974)
--
-- These are distinct code points, yet are normalized to the same "\969\769".
-- This means, say, doesFileExist on one will return true for the other.
--
-- This violates the tests here, as we are trying to conjure unique paths.
-- Thus we need to only generate paths whose normalization differ, at least
-- on OSX.
--
-- Should we do anything more e.g. update Charon to somehow take normalization
-- into account? It is tempting to say yes, yet consider the following two
-- laws we would really like Charon to follow.
--
-- 1. Delete p *> Restore p === no-op
-- 2. InfoFile (deleted p) = p.ext, when possible.
--
-- The first law rules out modifying the path on our end i.e. we really should
-- not be altering any paths given to us.
--
-- We could normalize the info files for backends that use the p.ext info file
-- strategy. But there doesn't seem to be any point to doing this, as info
-- files are not guaranteed to be match the original file anyway (i.e. real
-- duplicates).

type PathWithType = (OsPath, PathTypeW)

-- | Uses for equality checks so that we do not generate multiple paths with
-- the same normalization. See NOTE: [Unicode normalization].
newtype NormedFp = MkNormedFp {unNormedFilePath :: Text}
  deriving stock (Eq, Show)
  deriving (Hashable) via Text

makeFieldLabelsNoPrefix ''NormedFp

fpToNormedFp :: FilePath -> NormedFp
fpToNormedFp = MkNormedFp . TNormalize.normalize NFD . T.pack

normedFpToFp :: NormedFp -> FilePath
normedFpToFp = T.unpack . view #unNormedFilePath

-- | Holds all generated path data used for our tests.
data PathIntData = MkPathIntData
  { -- | Encoded FilePath.
    osPath :: OsPath,
    -- | Generated PathType.
    pathType :: PathTypeW,
    -- | Generated FilePath.
    filePath :: FilePath,
    -- | Normalized FilePath. See NOTE: [Unicode normalization]
    normed :: NormedFp
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''PathIntData

instance Eq PathIntData where
  x == y = x ^. #normed == y ^. #normed

instance Hashable PathIntData where
  hashWithSalt s x = hashWithSalt s (x ^. #normed)

-- | Type of generated data.
type GPaths = UniqueSeqNE PathIntData

genFileNameSet :: (MonadGen m) => Bool -> m GPaths
genFileNameSet asciiOnly =
  -- USeqNE.fromNonEmpty handles removing duplicates, as the generated
  -- PathIntData defines equality on normalized paths.
  USeqNE.fromNonEmpty <$> Gen.nonEmpty seqRange (genFileName asciiOnly)

gen2FileNameSets :: (MonadGen m) => Bool -> m (GPaths, GPaths)
gen2FileNameSets asciiOnly = do
  α <- genFileNameSet asciiOnly
  (\r -> (α, USeqNE.fromNonEmpty r))
    <$> Gen.nonEmpty seqRange (genFileNameNoDupes asciiOnly α)

gen3FileNameSets :: (MonadGen m) => Bool -> m (GPaths, GPaths, GPaths)
gen3FileNameSets asciiOnly = do
  (α, β) <- gen2FileNameSets asciiOnly
  (\r -> (α, β, USeqNE.fromNonEmpty r))
    <$> Gen.nonEmpty seqRange (genFileNameNoDupes asciiOnly (α `USeqNE.union` β))

seqRange :: Range Int
seqRange = Range.linear 1 100

genFileName ::
  (MonadGen m) =>
  Bool ->
  m PathIntData
genFileName asciiOnly = do
  pathType <-
    MkPathTypeW
      <$> Gen.element
        [ PathTypeFile,
          PathTypeDirectory,
          PathTypeSymbolicLink
        ]

  mkPathIntData pathType
    <$> Gen.string range (TestUtils.genPathChar asciiOnly)
  where
    range = Range.linear 1 20

genFileNameNoDupes ::
  (MonadGen m) =>
  Bool ->
  UniqueSeqNE PathIntData ->
  m PathIntData
genFileNameNoDupes asciiOnly paths = do
  pathType <-
    MkPathTypeW
      <$> Gen.element
        [ PathTypeFile,
          PathTypeDirectory,
          PathTypeSymbolicLink
        ]

  Gen.filterT
    (\x -> not (USeqNE.member x paths))
    ( mkPathIntData pathType
        <$> Gen.string range (TestUtils.genPathChar asciiOnly)
    )
  where
    range = Range.linear 1 20

mkPathIntData :: PathTypeW -> FilePath -> PathIntData
mkPathIntData pt fp =
  MkPathIntData
    { osPath = FsUtils.unsafeEncodeFpToValidOs fp,
      pathType = pt,
      filePath = fp,
      normed = fpToNormedFp fp
    }
