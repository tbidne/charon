module Unit.Data.PathData
  ( tests,
  )
where

import Data.Fixed (Fixed (MkFixed))
import Data.Time (LocalTime (LocalTime), TimeOfDay (..))
import GHC.Real ((^))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathData.Internal
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..))
import SafeRm.Data.Timestamp (Timestamp (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.PathData"
    [ serializeRoundtrip
    ]

serializeRoundtrip :: TestTree
serializeRoundtrip =
  testPropertyNamed "decode . encode ~ id" "serializeRoundtrip" $ do
    property $ do
      pathData@(UnsafePathData _ fileName _ _ _) <- forAll genPathData
      -- NOTE:
      -- Two caveats on injectivity:
      --
      -- 1. The fileName is thrown away. However this is intentional as we
      -- want it in the actual file name only (otherwise it is redundant).
      --
      -- We can thus think of the actual injection in terms of the implicit
      -- file name that is created by the encoding.
      --
      -- 2. We lose precision in the created timestamp, as we encode to second
      -- precision whereas the timestamp field technically has picosecond
      -- resolution. This is acceptable, as we do not care about
      -- precision > second.
      let encoded = PathData.encode pathData
          decoded = PathData.decode fileName encoded

      Right pathData === decoded

genPathData :: Gen PathData
genPathData =
  UnsafePathData
    <$> genPathType
    <*> genFileName
    <*> genOriginalPath
    <*> genSize
    <*> genTimestamp
  where
    genFileName = MkPathI <$> Gen.string (Range.exponential 1 100) Gen.unicode
    genOriginalPath = MkPathI <$> Gen.string (Range.linear 1 100) Gen.unicode
    genPathType = Gen.element [PathTypeDirectory, PathTypeFile]
    genSize = MkBytes <$> Gen.integral (Range.exponential 0 1_000_000_000_000)

genTimestamp :: Gen Timestamp
genTimestamp = MkTimestamp <$> genLocalTime
  where
    genLocalTime =
      LocalTime
        <$> genDay
        <*> genTimeOfDay
    -- 100_000 refers to 2132, probably future-proof enough :-)
    genDay = toEnum <$> Gen.integral (Range.linear 0 100_000)
    genTimeOfDay =
      TimeOfDay
        <$> Gen.integral (Range.linear 0 23)
        <*> Gen.integral (Range.linear 0 59)
        <*> (MkFixed <$> genSec)

    -- The roundabout way of generating picoseconds is due to needing a whole
    -- number of seconds; fractions of a second will cause the tests to fail
    -- due to the lack of > second precision, mentioned above
    genSec = (\x -> x * (10 ^ picoExp)) <$> Gen.integral (Range.linear 0 59)

    picoExp :: Integer
    picoExp = 12