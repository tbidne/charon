module Unit.Data.PathData
  ( tests,
  )
where

import Data.Fixed (Fixed (MkFixed))
import Data.Text.Lazy qualified as TL
import Data.Time (LocalTime (LocalTime), TimeOfDay (..))
import GHC.Real ((^))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.PathData (PathData (..))
import SafeRm.Data.PathData.Default qualified as Default
import SafeRm.Data.PathData.Fdo qualified as Fdo
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..))
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Data.Timestamp (Timestamp (..))
import SafeRm.Data.Timestamp qualified as Timestamp
import Text.Pretty.Simple qualified as Pretty
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.PathData"
    ( serializeRoundtripProp
        : [serializeRoundtripSpecs b specs | b <- backends]
    )
  where
    backends = [minBound .. maxBound]
    specs =
      [ ("1", "\t"),
        ("2", "path\\nnew\\n\\nlines"),
        ("3", "path\\n\\n")
      ]

serializeRoundtripSpecs :: Backend -> [(String, String)] -> TestTree
serializeRoundtripSpecs backend params = testCase desc $ do
  ts <- Timestamp.fromText "1858-11-17T00:00:00"

  for_ params $ \(fileName, originalPath) -> do
    let pd = backendToMk backend fileName originalPath ts
        encoded = encode pd

    case decode (backend, MkPathI fileName) encoded of
      Left err ->
        assertFailure $
          mconcat
            [ "PathData:\n\n",
              TL.unpack (Pretty.pShow pd),
              "\n\nEncoded:\n\n",
              bsToStrLenient encoded,
              "\n\nError: ",
              err
            ]
      Right decoded -> pd @=? decoded
  where
    desc = "decode . encode ~ id (specs) " ++ Backend.backendTestDesc backend
    backendToMk BackendDefault name opath ts =
      PathDataDefault $
        Default.UnsafePathData
          { pathType = PathTypeFile,
            fileName = MkPathI name,
            originalPath = MkPathI opath,
            size = MkBytes 0,
            created = ts
          }
    backendToMk BackendFdo name opath ts =
      PathDataFdo $
        Fdo.UnsafePathData
          { fileName = MkPathI name,
            originalPath = MkPathI opath,
            created = ts
          }

serializeRoundtripProp :: TestTree
serializeRoundtripProp =
  testPropertyNamed "decode . encode ~ id (prop)" "serializeRoundtripProp" $ do
    property $ do
      (pathData, backend) <- forAll genPathData
      let fileName = pathData ^. #fileName
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
      let encoded = encode pathData
          decoded = decode (backend, fileName) encoded

      annotateShow encoded
      annotateShow decoded

      Right pathData === decoded

genPathData :: Gen (PathData, Backend)
genPathData =
  Gen.choice
    [ (\x -> (PathDataDefault x, BackendDefault)) <$> genDefaultPathData,
      (\x -> (PathDataFdo x, BackendFdo)) <$> genFdoPathData
    ]

genDefaultPathData :: Gen Default.PathData
genDefaultPathData =
  Default.UnsafePathData
    <$> genPathType
    <*> genFileName
    <*> genOriginalPath
    <*> genSize
    <*> genTimestamp
  where
    genFileName = MkPathI <$> Gen.string (Range.exponential 1 100) genPathChar
    genOriginalPath = MkPathI <$> Gen.string (Range.linear 1 100) genPathChar
    genPathType = Gen.element [PathTypeDirectory, PathTypeFile]
    genSize = MkBytes <$> Gen.integral (Range.exponential 0 1_000_000_000_000)

genFdoPathData :: Gen Fdo.PathData
genFdoPathData =
  Fdo.UnsafePathData
    <$> genFileName
    <*> genOriginalPath
    <*> genTimestamp
  where
    genFileName = MkPathI <$> Gen.string (Range.exponential 1 100) genPathChar
    genOriginalPath = MkPathI <$> Gen.string (Range.linear 1 100) genPathChar

genPathChar :: Gen Char
genPathChar = Gen.filter (/= '\NUL') Gen.unicode

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
