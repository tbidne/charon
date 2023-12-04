{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for integration test suite.
module Integration.Utils
  ( -- * Types
    GPaths,
    PathWithType (..),
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
import Data.Hashable (Hashable (hash))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Normalize (NormalizationMode (NFD))
import Data.Text.Normalize qualified as TNormalize
import Effects.FileSystem.Utils qualified as FsUtils
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import Integration.Prelude
import Test.Utils qualified as TestUtils

-- | Generated path name along with the type to create.
newtype PathWithType = MkPathWithType {unPathWithType :: (OsPath, PathTypeW)}
  deriving stock (Show)

makeFieldLabelsNoPrefix ''PathWithType

instance Eq PathWithType where
  MkPathWithType (p1, _) == MkPathWithType (p2, _) = p1 == p2

instance Ord PathWithType where
  MkPathWithType (p1, _) <= MkPathWithType (p2, _) = p1 <= p2

instance Hashable PathWithType where
  hashWithSalt i (MkPathWithType (p1, _)) = hashWithSalt i p1
  hash (MkPathWithType (p1, _)) = hash p1

-- | Type of generated paths. Includes the original paths before encoding for
-- easier debugging.
type GPaths = (UniqueSeqNE PathWithType, UniqueSeqNE NormedFp)

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
newtype NormedFp = MkNormedFp {unNormedFilePath :: Text}
  deriving stock (Eq, Show)
  deriving (Hashable) via Text

makeFieldLabelsNoPrefix ''NormedFp

fpToNormedFp :: FilePath -> NormedFp
fpToNormedFp = MkNormedFp . TNormalize.normalize NFD . T.pack

normedFpToFp :: NormedFp -> FilePath
normedFpToFp = T.unpack . view #unNormedFilePath

genFileNameSet :: (MonadGen m) => Bool -> m GPaths
genFileNameSet asciiOnly =
  splitPaths <$> Gen.nonEmpty seqRange (genFileName asciiOnly)

gen2FileNameSets :: (MonadGen m) => Bool -> m (GPaths, GPaths)
gen2FileNameSets asciiOnly = do
  α@(_, αFps) <- genFileNameSet asciiOnly
  (\r -> (α, splitPaths r))
    <$> Gen.nonEmpty seqRange (genFileNameNoDupes asciiOnly αFps)

gen3FileNameSets :: (MonadGen m) => Bool -> m (GPaths, GPaths, GPaths)
gen3FileNameSets asciiOnly = do
  (α@(_, αFps), β@(_, βFps)) <- gen2FileNameSets asciiOnly
  (\r -> (α, β, splitPaths r))
    <$> Gen.nonEmpty seqRange (genFileNameNoDupes asciiOnly (αFps `USeqNE.union` βFps))

splitPaths :: NonEmpty (PathWithType, NormedFp) -> GPaths
splitPaths = bimap USeqNE.fromNonEmpty USeqNE.fromNonEmpty . NE.unzip

seqRange :: Range Int
seqRange = Range.linear 1 100

genFileName ::
  (MonadGen m) =>
  Bool ->
  m (PathWithType, NormedFp)
genFileName asciiOnly = do
  pathType <-
    MkPathTypeW
      <$> Gen.element
        [ PathTypeFile,
          PathTypeDirectory,
          PathTypeSymbolicLink
        ]

  (\fp -> (MkPathWithType (FsUtils.unsafeEncodeFpToValidOs fp, pathType), fpToNormedFp fp))
    <$> Gen.string range (TestUtils.genPathChar asciiOnly)
  where
    range = Range.linear 1 20

genFileNameNoDupes ::
  (MonadGen m) =>
  Bool ->
  UniqueSeqNE NormedFp ->
  m (PathWithType, NormedFp)
genFileNameNoDupes asciiOnly paths = do
  pathType <-
    MkPathTypeW
      <$> Gen.element
        [ PathTypeFile,
          PathTypeDirectory,
          PathTypeSymbolicLink
        ]

  (\fp -> (MkPathWithType (FsUtils.unsafeEncodeFpToValidOs fp, pathType), fpToNormedFp fp))
    <$> Gen.filterT
      (\fp -> not (USeqNE.member (fpToNormedFp fp) paths))
      (Gen.string range (TestUtils.genPathChar asciiOnly))
  where
    range = Range.linear 1 20
