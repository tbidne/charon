-- | Prelude for integration test suite.
module Integration.Utils
  ( -- * Types
    GPaths,
    PathWithType (..),
    unPathWithType,

    -- * Gen
    genFileNameSet,
    gen2FileNameSets,
    gen3FileNameSets,
  )
where

import Data.Hashable (Hashable (hash))
import Data.List (unzip)
import Effects.FileSystem.Utils qualified as FsUtils
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import Integration.Prelude
import SafeRm.Data.PathType (PathTypeW (MkPathTypeW))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Data.UniqueSeq qualified as USeq
import Test.Utils qualified as TestUtils

-- | Generated path name along with the type to create.
newtype PathWithType = MkPathWithType (OsPath, PathTypeW)
  deriving stock (Show)

unPathWithType :: PathWithType -> OsPath
unPathWithType (MkPathWithType (p, _)) = p

instance Eq PathWithType where
  MkPathWithType (p1, _) == MkPathWithType (p2, _) = p1 == p2

instance Ord PathWithType where
  MkPathWithType (p1, _) <= MkPathWithType (p2, _) = p1 <= p2

instance Hashable PathWithType where
  hashWithSalt i (MkPathWithType (p1, _)) = hashWithSalt i p1
  hash (MkPathWithType (p1, _)) = hash p1

-- | Type of generated paths. Includes the original paths before encoding for
-- easier debugging.
type GPaths = (UniqueSeq PathWithType, UniqueSeq FilePath)

genFileNameSet :: (MonadGen m) => Bool -> m GPaths
genFileNameSet asciiOnly =
  splitPaths <$> Gen.list seqRange (genFileName asciiOnly)

gen2FileNameSets :: (MonadGen m) => Bool -> m (GPaths, GPaths)
gen2FileNameSets asciiOnly = do
  α@(_, αFps) <- genFileNameSet asciiOnly
  (\r -> (α, splitPaths r))
    <$> Gen.list seqRange (genFileNameNoDupes asciiOnly αFps)

gen3FileNameSets :: (MonadGen m) => Bool -> m (GPaths, GPaths, GPaths)
gen3FileNameSets asciiOnly = do
  (α@(_, αFps), β@(_, βFps)) <- gen2FileNameSets asciiOnly
  (\r -> (α, β, splitPaths r))
    <$> Gen.list seqRange (genFileNameNoDupes asciiOnly (αFps `USeq.union` βFps))

splitPaths :: [(PathWithType, FilePath)] -> GPaths
splitPaths = bimap fromFoldable fromFoldable . unzip

seqRange :: Range Int
seqRange = Range.linear 1 100

genFileName ::
  (MonadGen m) =>
  Bool ->
  m (PathWithType, FilePath)
genFileName asciiOnly = genFileNameNoDupes asciiOnly USeq.empty

genFileNameNoDupes ::
  (MonadGen m) =>
  Bool ->
  UniqueSeq FilePath ->
  m (PathWithType, FilePath)
genFileNameNoDupes asciiOnly paths = do
  pathType <-
    MkPathTypeW
      <$> Gen.element
        [ PathTypeFile,
          PathTypeDirectory,
          PathTypeSymbolicLink
        ]

  (\fp -> (MkPathWithType (FsUtils.unsafeEncodeFpToValidOs fp, pathType), fp))
    <$> Gen.filterT
      (\fp -> not (USeq.member fp paths))
      (Gen.string range (TestUtils.genPathChar asciiOnly))
  where
    range = Range.linear 1 20
