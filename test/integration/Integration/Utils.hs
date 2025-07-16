{-# LANGUAGE CPP #-}
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

    -- * Helpers
    mkPathIntData,

    -- * Tests
    tests,
  )
where

import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.UniqueSeqNE (UniqueSeqNE, (∉))
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Data.Text qualified as T
import FileSystem.OsPath qualified as OsPath
import FileSystem.UTF8 qualified as UTF8
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
-- This means, say, doesFileExist on one will return the same as the other.
--
-- This violates the tests here, as we were assuming unique utf-8 sequences
-- would mean unique paths. Thus we need to generate paths whose
-- normalizations differ, at least on OSX.
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
--
-- We could warn users when they try to delete two different byte sequences
-- whose normalizations coincide, but this requires more thought e.g.
--
--   1. When do we check? Every time a user hands us paths, or just e.g. delete?
--   2. Do we have different behavior / warnings for different Os's?
--   3. What do we do if a user tries to restore a path p1 by typing p2 s.t.
--      p2 /= p1 but norm p2 == norm p1? Right now we do nothing i.e. lookup
--      will fail.
--
-- For now we do not do anything except fix the tests and make a note of it.
--
-- UPDATE 2024-02-13: Apparently, the above normalization is __still__ not
-- enough (good lord apple). OSX also considers some paths equal even if their
-- normalizations do not agree. Which ones? Who knows! Trying to find
-- reliable info has proven difficult.
--
-- In the meantime, we make our normalization logic even stronger (on OSX)
-- by including case folding.

type PathWithType = (OsPath, PathTypeW)

-- | Uses for equality checks so that we do not generate multiple paths with
-- the same normalization. See NOTE: [Unicode normalization].
newtype NormedFp = MkNormedFp {unNormedFilePath :: Text}
  deriving stock (Eq, Show)
  deriving (Hashable) via Text

makeFieldLabelsNoPrefix ''NormedFp

fpToNormedFp :: FilePath -> NormedFp
#if OSX
-- Include case folding because e.g. OSX considers the paths
-- U+1c81 (ᲁ) and U+0434 (д) equal despite their normalizations differing.
-- Thankfully these share the same case folding.
--
-- REVIEW: Does the order of caseFold and normalization matter (probably...)?
fpToNormedFp = MkNormedFp . T.toCaseFold . UTF8.normalizeC . T.pack
#else
fpToNormedFp = MkNormedFp . UTF8.normalizeC . T.pack
#endif

normedFpToFp :: NormedFp -> FilePath
normedFpToFp = T.unpack . view #unNormedFilePath

-- | Holds all generated path data used for our int tests.
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
          PathTypeOther,
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
          PathTypeOther,
          PathTypeSymbolicLink
        ]

  Gen.filterT
    (∉ paths)
    ( mkPathIntData pathType
        <$> Gen.string range (TestUtils.genPathChar asciiOnly)
    )
  where
    range = Range.linear 1 20

mkPathIntData :: PathTypeW -> FilePath -> PathIntData
mkPathIntData pt fp =
  MkPathIntData
    { osPath = OsPath.unsafeEncodeValid fp,
      pathType = pt,
      filePath = fp,
      normed = fpToNormedFp fp
    }

tests :: TestTree
tests =
  testGroup
    "Integration.Utils"
    $ testNormalizedEq
    : osxTests

testNormalizedEq :: TestTree
testNormalizedEq = testCase "Equal normalization => Eq" $ do
  --        U+1F7D: ώ             U+03CE: ώ
  normalize "\8061" @=? normalize "\974"

osxTests :: [TestTree]
#if OSX
osxTests = [ testCaseFoldingEq ]

testCaseFoldingEq :: TestTree
testCaseFoldingEq = testCase "Equal case folding => Eq" $ do
  --        U+1c81: ᲁ             U+0434: д
  normalize "\7297" @=? normalize "\1076"

#else
osxTests = []
#endif

normalize :: FilePath -> PathIntData
normalize = mkPathIntData (MkPathTypeW PathTypeFile)
