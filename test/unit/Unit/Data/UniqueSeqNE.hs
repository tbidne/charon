-- | Unit tests for Data.UniqueSeq
module Unit.Data.UniqueSeqNE
  ( tests,
  )
where

import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Data.UniqueSeqNE.Internal (UniqueSeqNE (MkUniqueSeqNE))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty))
import Data.Sequence.NonEmpty qualified as NESeq
import GHC.Exts (IsList (toList))
import Hedgehog (PropertyT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unit.Prelude
import Utils qualified

tests :: TestTree
tests =
  testGroup
    "Data.UniqueSeq"
    [ invariantTests,
      lawsTests
    ]

invariantTests :: TestTree
invariantTests =
  testGroup
    "General Invariants"
    [ isListIsomorphism,
      isListOrder,
      fromFoldable1Order,
      unionOrder,
      mapInvariant,
      insertInvariant
    ]

isListIsomorphism :: TestTree
isListIsomorphism =
  testPropertyNamed "fromList . toList === id" "isListIsomorphism" $ do
    property $ do
      useq <- forAll genUniqueSeq
      let xs = useq ^. #seq
      annotateShow xs

      useq === USeqNE.fromFoldable1 xs

isListOrder :: TestTree
isListOrder =
  testPropertyNamed "toList . fromList preserves order" "isListOrder" $ do
    property $ do
      origList <- forAll genUniqueList
      let useq = USeqNE.fromFoldable1 origList
      annotateShow useq

      Utils.assertSameOrder (toList origList) (useqNeToList useq)

fromFoldable1Order :: TestTree
fromFoldable1Order =
  testPropertyNamed "fromFoldable1 preserves order" "fromFoldable1Order" $ do
    property $ do
      xs <- forAll genUniqueList
      let useq@(MkUniqueSeqNE seq _) = USeqNE.fromFoldable1 xs

      annotateShow seq
      sameOrder (toList xs) (NESeq.toSeq seq)

      uniqseqInvariants useq
  where
    sameOrder [] Empty = pure ()
    sameOrder (x : _) Empty = annotateShow x *> failure
    sameOrder [] (y :<| _) = annotateShow y *> failure
    sameOrder (x : xs) (y :<| ys) = do
      x === y
      sameOrder xs ys

unionOrder :: TestTree
unionOrder =
  testPropertyNamed "union preserves order" "unionOrder" $ do
    property $ do
      xs <- forAll genUniqueList
      ys <- forAll genUniqueList
      let xuseq = USeqNE.fromFoldable1 xs
          yuseq = USeqNE.fromFoldable1 ys

          uxy = USeqNE.union xuseq yuseq
          uyx = USeqNE.union yuseq xuseq

      annotateShow xuseq
      annotateShow yuseq
      annotateShow uxy
      annotateShow uyx

      Utils.assertSameOrder (toList $ xs <> ys) (useqNeToList uxy)
      Utils.assertSameOrder (toList $ ys <> xs) (useqNeToList uyx)

      uniqseqInvariants uxy
      uniqseqInvariants uyx

mapInvariant :: TestTree
mapInvariant =
  testPropertyNamed "map invariants" "mapInvariant" $ do
    property $ do
      useq <- forAll genUniqueSeq
      let useq' = USeqNE.map even useq
      uniqseqInvariants useq'

insertInvariant :: TestTree
insertInvariant =
  testPropertyNamed "insert invariants" "insertInvariant" $ do
    property $ do
      xs <- forAll genList
      useqNE <- forAll genUniqueSeq
      let useqPrepend = foldr USeqNE.prepend useqNE xs
          useqAppend = foldl' USeqNE.append useqNE xs

      uniqseqInvariants useqPrepend
      uniqseqInvariants useqAppend

lawsTests :: TestTree
lawsTests =
  testGroup
    "Laws"
    [ semigroup,
      insertMember
    ]

semigroup :: TestTree
semigroup =
  testPropertyNamed "union is a semigroup" "semigroup" $ do
    property $ do
      a <- forAll genUniqueSeq
      b <- forAll genUniqueSeq
      c <- forAll genUniqueSeq

      annotate "Associativity"
      (a `USeqNE.union` b) `USeqNE.union` c === a `USeqNE.union` (b `USeqNE.union` c)

insertMember :: TestTree
insertMember =
  testPropertyNamed "x ∈ insert x useq" "insertMember" $ do
    property $ do
      useq <- forAll genUniqueSeq
      x <- forAll genInt

      let useqA = USeqNE.append useq x
          useqP = USeqNE.prepend x useq

      annotateShow useqA
      assert $ USeqNE.member x useqA

      annotateShow useqP
      assert $ USeqNE.member x useqP

uniqseqInvariants :: (Hashable a, Show a) => UniqueSeqNE a -> PropertyT IO ()
uniqseqInvariants useq = do
  foundRef <- liftIO $ newIORef HSet.empty
  seqAndSetSynced useq
  seqUnique foundRef useq

seqAndSetSynced :: (Hashable a, Show a) => UniqueSeqNE a -> PropertyT IO ()
seqAndSetSynced (MkUniqueSeqNE seq set) = do
  annotateShow seq
  annotateShow set
  -- same size
  length seq === length set

  -- all seq in set
  for_ seq $ \x -> do
    annotateShow x
    assert $ HSet.member x set

seqUnique ::
  forall a.
  (Hashable a, Show a) =>
  IORef (HashSet a) ->
  UniqueSeqNE a ->
  PropertyT IO ()
seqUnique foundRef (MkUniqueSeqNE seq _) = foldr go (pure ()) seq
  where
    go :: a -> PropertyT IO () -> PropertyT IO ()
    go x acc = do
      found <- liftIO $ readIORef foundRef
      if HSet.member x found
        then do
          annotate "Found duplicate"
          annotateShow x
          failure
        else do
          liftIO $ modifyIORef' foundRef (HSet.insert x)
          acc

genUniqueSeq :: Gen (UniqueSeqNE Int)
genUniqueSeq = USeqNE.fromFoldable1 <$> genList

genUniqueList :: Gen (NonEmpty Int)
genUniqueList = do
  x :| xs <- genList
  let (_, uniq) = foldl' go (HSet.singleton x, []) xs
  pure (x :| uniq)
  where
    go :: (HashSet Int, [Int]) -> Int -> (HashSet Int, [Int])
    go (found, acc) y
      | not (HSet.member y found) = (HSet.insert y found, y : acc)
      | otherwise = (found, acc)

genList :: Gen (NonEmpty Int)
genList = Gen.nonEmpty listRange genInt
  where
    listRange = Range.exponential 1 10_000

genInt :: Gen Int
genInt = Gen.integral intRange
  where
    intRange = Range.linearFrom 0 minBound maxBound

useqNeToList :: UniqueSeqNE a -> [a]
useqNeToList = neSeqToList . view #seq

neSeqToList :: NESeq a -> [a]
neSeqToList (x :<|| xs) = x : toList xs
