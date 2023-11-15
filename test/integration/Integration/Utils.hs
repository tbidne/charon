{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Prelude for integration test suite.
module Integration.Utils
  ( GPaths,
    genFileNameSet,
    gen2FileNameSets,
    gen3FileNameSets,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Bifunctor (Bifunctor (first))
import Data.Char qualified as Ch
import Data.HashSet qualified as HSet
import Data.Hashable (Hashable (hash))
import Data.List (unzip)
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.LoggerNS (Namespace, defaultLogFormatter)
import Effects.LoggerNS qualified as Logger
import GHC.Exts (IsList (Item, fromList))
import Hedgehog (GenT, PropertyT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import Integration.AsciiOnly (AsciiOnly)
import Integration.Prelude
import SafeRm qualified
import SafeRm.Backend.Data (Backend)
import SafeRm.Backend.Data qualified as Backend
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Data.UniqueSeq qualified as USeq
import SafeRm.Env (HasBackend, HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Exception (FileNotFoundE, TrashEntryNotFoundE)
import SafeRm.Runner.Env
  ( Env,
    LogEnv (MkLogEnv),
  )
import SafeRm.Runner.SafeRmT (SafeRmT)
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet), withGuard_)
import System.OsPath qualified as OsPath
import Test.Utils qualified as TestUtils

-- | Type of generated paths. Includes the original paths before encoding for
-- easier debugging.
type GPaths = (UniqueSeq OsPath, UniqueSeq FilePath)

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

splitPaths :: [(OsPath, FilePath)] -> GPaths
splitPaths = bimap fromFoldable fromFoldable . unzip

seqRange :: Range Int
seqRange = Range.linear 1 100

genFileName ::
  (MonadGen m) =>
  Bool ->
  m (OsPath, FilePath)
genFileName asciiOnly = genFileNameNoDupes asciiOnly USeq.empty

genFileNameNoDupes ::
  (MonadGen m) =>
  Bool ->
  UniqueSeq FilePath ->
  m (OsPath, FilePath)
genFileNameNoDupes asciiOnly paths =
  (\fp -> (FsUtils.unsafeEncodeFpToValidOs fp, fp))
    <$> Gen.filterT
      (\fp -> not (USeq.member fp paths))
      (Gen.string range (TestUtils.genPathChar asciiOnly))
  where
    range = Range.linear 1 20
