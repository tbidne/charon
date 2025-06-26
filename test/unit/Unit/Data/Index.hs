{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Charon.Data.Index (Index (MkIndex), Sort (Name, Size))
import Charon.Data.Index qualified as Index
import Charon.Data.PathData (PathData (UnsafePathData))
import Charon.Data.PathData.Formatting
  ( ColFormat (ColFormatFixed, ColFormatMax),
    Coloring (ColoringOff, ColoringOn),
    PathDataFormat
      ( FormatMultiline,
        FormatSingleline,
        FormatTabular,
        FormatTabularSimple
      ),
    _FormatTabular,
  )
import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import Charon.Data.Timestamp (Timestamp, fromText)
import Charon.Env (HasTrashHome (getTrashHome))
import Charon.Runner.Command.List
import Charon.Utils qualified as U
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import Effects.System.Terminal
  ( MonadTerminal (getTerminalSize),
    Window (Window, height, width),
  )
import FileSystem.OsPath (unsafeDecode, unsafeEncodeValid)
import System.OsPath qualified as FP
import Test.Utils qualified as TestUtils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.Index"
    [formattingTests]

formattingTests :: TestTree
formattingTests =
  testGroup
    "Formatting"
    [ multilineTests,
      singlelineTests,
      tabularSimpleTests,
      tabularFixedTests,
      tabularAutoTests,
      tabularMaxTests,
      tabularMiscTests
    ]

multilineTests :: TestTree
multilineTests =
  testGroup
    "Multiline"
    [ testFormatMultiline1,
      testFormatMultiline2,
      testFormatMultiline3,
      testFormatMultiline4
    ]

singlelineTests :: TestTree
singlelineTests =
  testGroup
    "Singeline"
    [ testFormatSingleline1,
      testFormatSingleline2,
      testFormatSingleline3,
      testFormatSingleline4,
      testFormatSinglelineColor
    ]

tabularSimpleTests :: TestTree
tabularSimpleTests =
  testGroup
    "Tabular Simple"
    [ testFormatTabularSimple1,
      testFormatTabularSimple2,
      testFormatTabularSimple3,
      testFormatTabularSimple4,
      testFormatTabularSimpleColor
    ]

-- Tests w/ fixed format lengths, basically verifying the other args like
-- the multiline tests.
tabularFixedTests :: TestTree
tabularFixedTests =
  testGroup
    "Tabular fixed"
    [ testFormatTabularFixed1,
      testFormatTabularFixed2,
      testFormatTabularFixed3,
      testFormatTabularFixed4,
      testFormatTabularFixedColor
    ]

testFormatMultiline1 :: TestTree
testFormatMultiline1 =
  testGoldenFormatParams
    "Multiline, name, asc"
    [osp|multi-name-asc|]
    FormatMultiline
    Name
    False

testFormatMultiline2 :: TestTree
testFormatMultiline2 =
  testGoldenFormatParams
    "Multiline, name, desc"
    [osp|multi-name-desc|]
    FormatMultiline
    Name
    True

testFormatMultiline3 :: TestTree
testFormatMultiline3 =
  testGoldenFormatParams
    "Multiline, size, asc"
    [osp|multi-size-asc|]
    FormatMultiline
    Size
    False

testFormatMultiline4 :: TestTree
testFormatMultiline4 =
  testGoldenFormatParams
    "Multiline, size, desc"
    [osp|multi-size-desc|]
    FormatMultiline
    Size
    True

testFormatSingleline1 :: TestTree
testFormatSingleline1 =
  testGoldenFormatParams
    "Singleline, name, asc"
    [osp|single-name-asc|]
    (FormatSingleline ColoringOff)
    Name
    False

testFormatSingleline2 :: TestTree
testFormatSingleline2 =
  testGoldenFormatParams
    "Singleline, name, desc"
    [osp|single-name-desc|]
    (FormatSingleline ColoringOff)
    Name
    True

testFormatSingleline3 :: TestTree
testFormatSingleline3 =
  testGoldenFormatParams
    "Singleline, size, asc"
    [osp|single-size-asc|]
    (FormatSingleline ColoringOff)
    Size
    False

testFormatSingleline4 :: TestTree
testFormatSingleline4 =
  testGoldenFormatParams
    "Singleline, size, desc"
    [osp|single-size-desc|]
    (FormatSingleline ColoringOff)
    Size
    True

testFormatSinglelineColor :: TestTree
testFormatSinglelineColor =
  testGoldenFormatParams
    "Singleline, size, desc, color"
    [osp|single-size-desc-color|]
    (FormatSingleline ColoringOn)
    Size
    True

testFormatTabularSimple1 :: TestTree
testFormatTabularSimple1 =
  testGoldenFormatParams
    "TabularSimple, name, asc"
    [osp|tabular-simple-name-asc|]
    (FormatTabularSimple ColoringOff)
    Name
    False

testFormatTabularSimple2 :: TestTree
testFormatTabularSimple2 =
  testGoldenFormatParams
    "TabularSimple, name, desc"
    [osp|tabular-simple-name-desc|]
    (FormatTabularSimple ColoringOff)
    Name
    True

testFormatTabularSimple3 :: TestTree
testFormatTabularSimple3 =
  testGoldenFormatParams
    "TabularSimple, size, asc"
    [osp|tabular-simple-size-asc|]
    (FormatTabularSimple ColoringOff)
    Size
    False

testFormatTabularSimple4 :: TestTree
testFormatTabularSimple4 =
  testGoldenFormatParams
    "TabularSimple, size, desc"
    [osp|tabular-simple-size-desc|]
    (FormatTabularSimple ColoringOff)
    Size
    True

testFormatTabularSimpleColor :: TestTree
testFormatTabularSimpleColor =
  testGoldenFormatParams
    "TabularSimple, size, desc, color"
    [osp|tabular-simple-size-desc-color|]
    (FormatTabularSimple ColoringOn)
    Size
    True

testFormatTabularFixed1 :: TestTree
testFormatTabularFixed1 =
  testGoldenFormatParams
    "Tabular, name, asc"
    [osp|tabular-name-asc|]
    fixedTabularFormat
    Name
    False

testFormatTabularFixed2 :: TestTree
testFormatTabularFixed2 =
  testGoldenFormatParams
    "Tabular, name, desc"
    [osp|tabular-name-desc|]
    fixedTabularFormat
    Name
    True

testFormatTabularFixed3 :: TestTree
testFormatTabularFixed3 =
  testGoldenFormatParams
    "Tabular, size, asc"
    [osp|tabular-size-asc|]
    fixedTabularFormat
    Size
    False

testFormatTabularFixed4 :: TestTree
testFormatTabularFixed4 =
  testGoldenFormatParams
    "Tabular, size, desc"
    [osp|tabular-size-desc|]
    fixedTabularFormat
    Size
    True

testFormatTabularFixedColor :: TestTree
testFormatTabularFixedColor =
  testGoldenFormatParams
    "Tabular, size, desc, color"
    [osp|tabular-size-desc-color|]
    (set' (_FormatTabular % _1) ColoringOn fixedTabularFormat)
    Size
    True

fixedTabularFormat :: PathDataFormat
fixedTabularFormat =
  FormatTabular
    ColoringOff
    (Just $ ColFormatFixed 10)
    (Just $ ColFormatFixed 22)

data TestEnv = MkTestEnv Natural (PathI TrashHome)

instance HasTrashHome TestEnv where
  getTrashHome (MkTestEnv _ th) = th

newtype ConfigIO a = MkConfigIO (ReaderT TestEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIO,
      MonadIORef,
      MonadPosixCompat,
      MonadReader TestEnv,
      MonadThrow
    )
    via ReaderT TestEnv IO

#if !WINDOWS
deriving newtype instance MonadPosix ConfigIO
#endif

instance MonadPathReader ConfigIO where
  listDirectory _ = pure []
  pathIsSymbolicLink _ = pure False

  doesFileExist = pure . not . (`L.elem` dirs) . FP.takeFileName
  doesDirectoryExist = pure . (`L.elem` dirs) . FP.takeFileName

  getFileSize p =
    if
      | name == [osp|foo|] -> pure 70
      | name == [osp|bazzz|] -> pure 5_000
      | name == [osp|dir|] -> pure 20_230
      | name == [osp|f|] -> pure 13_070_000
      | name == [osp|z|] -> pure 200_120
      | name == [osp|d|] -> pure 55_000_000_000_000_000_000_000_000_000
      | name == unsafeEncodeValid (L.replicate 50 'b') -> pure 10
      | otherwise -> error $ "getFileSize: " <> show p
    where
      name = FP.takeFileName p

dirs :: [OsPath]
dirs = [[osp|dir|], [osp|d|]]

runConfigIO :: ConfigIO a -> Natural -> IO a
runConfigIO (MkConfigIO x) = runReaderT x . (`MkTestEnv` MkPathI trashPath)

trashPath :: OsPath
trashPath = [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|]

instance MonadTerminal ConfigIO where
  getTerminalSize =
    ask >>= \(MkTestEnv n _) ->
      pure
        $ Window
          { height = 50,
            width = fromIntegral n
          }
  putStr = liftIO . putStr

instance MonadLogger ConfigIO where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNS ConfigIO where
  getNamespace = pure ""
  localNamespace _ m = m

-- Tests tabular automatic formatting i.e. nothing specified
tabularAutoTests :: TestTree
tabularAutoTests =
  testGroup
    "Tabular auto"
    [ testFormatTabularAutoNormal,
      testFormatTabularAutoMinTermSize,
      testFormatTabularAutoApprox,
      testFormatTabularAutoEmpty,
      formatTabularAutoFail
    ]

testFormatTabularAutoNormal :: TestTree
testFormatTabularAutoNormal = testGoldenFormat desc fileName mkIndex formatTabularAuto 100
  where
    desc = "Auto tabular format"
    fileName = [osp|tabular-auto-normal|]

testFormatTabularAutoMinTermSize :: TestTree
testFormatTabularAutoMinTermSize = testGoldenFormat desc fileName mkIndex formatTabularAuto 59
  where
    desc = "Auto tabular formats minimum terminal size"
    fileName = [osp|tabular-auto-min|]

testFormatTabularAutoApprox :: TestTree
testFormatTabularAutoApprox = testGoldenFormat desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular falls back to estimates for large paths"
    fileName = [osp|tabular-auto-large-approx|]
    mkIdx = do
      ts <- fromText "2020-05-31T12:00:00"
      pure
        [ UnsafePathData
            (MkPathTypeW PathTypeFile)
            (MkPathI [osp|foo|])
            (MkPathI $ unsafeEncodeValid $ L.replicate 80 'f')
            (fromℤ 70)
            ts,
          UnsafePathData
            (MkPathTypeW PathTypeFile)
            (MkPathI $ unsafeEncodeValid $ L.replicate 50 'b')
            (MkPathI [osp|bar|])
            (fromℤ 10)
            ts
        ]

testFormatTabularAutoEmpty :: TestTree
testFormatTabularAutoEmpty = testGoldenFormat desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular empty"
    fileName = [osp|tabular-auto-empty|]
    mkIdx = pure []

formatTabularAutoFail :: TestTree
formatTabularAutoFail = testCase desc $ do
  let idx = MkIndex []
  eformatted <-
    trySync
      $ runConfigIO
        (Index.formatIndex (MkListCmd formatTabularAuto Name False) idx)
        53
  case eformatted of
    Right result ->
      assertFailure
        $ "Expected exception, received result: "
        <> show result
    Left ex ->
      assertBool (T.unpack $ U.displayExT ex) (expected `T.isPrefixOf` U.displayExT ex)
  where
    desc = "Auto tabular throws error for small terminal width"
    expected =
      mconcat
        [ "Terminal width (53) is less than minimum width (54) for automatic ",
          "tabular display. Perhaps try multiline."
        ]

-- Tests tabular w/ max options
tabularMaxTests :: TestTree
tabularMaxTests =
  testGroup
    "Tabular max"
    [ testFormatTabularMaxNameAutoOrig,
      testFormatTabularMaxNameAutoOrigTrunc,
      testFormatTabularAutoNameMaxOrig,
      testFormatTabularAutoNameMaxOrigTrunc,
      testFormatTabularMaxNameMaxOrig
    ]

testFormatTabularMaxNameAutoOrig :: TestTree
testFormatTabularMaxNameAutoOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig"
    fileName = [osp|tabular-max-name-auto-orig|]

testFormatTabularMaxNameAutoOrigTrunc :: TestTree
testFormatTabularMaxNameAutoOrigTrunc = testGoldenFormat desc fileName mkIndex fmt 70
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig truncates orig"
    fileName = [osp|tabular-max-name-auto-orig-trunc|]

testFormatTabularAutoNameMaxOrig :: TestTree
testFormatTabularAutoNameMaxOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path"
    fileName = [osp|tabular-auto-name-max-orig|]

testFormatTabularAutoNameMaxOrigTrunc :: TestTree
testFormatTabularAutoNameMaxOrigTrunc = testGoldenFormat desc fileName mkIndex fmt 70
  where
    fmt = FormatTabular ColoringOff Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path truncates name"
    fileName = [osp|tabular-auto-name-max-orig-trunc|]

testFormatTabularMaxNameMaxOrig :: TestTree
testFormatTabularMaxNameMaxOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) (Just ColFormatMax)
    desc = "Tabular max file name and max original path"
    fileName = [osp|tabular-max-name-max-orig|]

-- Misc tabular tests e.g. interactions between various options
tabularMiscTests :: TestTree
tabularMiscTests =
  testGroup
    "Tabular misc"
    [ testFormatTabularFixedNameMaxOrig,
      testFormatTabularFixedNameAutoOrig,
      testFormatTabularMaxNameFixedOrig,
      testFormatTabularAutoNameFixedOrig
    ]

testFormatTabularFixedNameMaxOrig :: TestTree
testFormatTabularFixedNameMaxOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just $ ColFormatFixed 50) (Just ColFormatMax)
    desc = "Tabular fixed file name and max original path"
    fileName = [osp|tabular-fix-name-max-orig|]

testFormatTabularFixedNameAutoOrig :: TestTree
testFormatTabularFixedNameAutoOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just $ ColFormatFixed 50) Nothing
    desc = "Tabular fixed file name and auto original path"
    fileName = [osp|tabular-fix-name-auto-orig|]

testFormatTabularMaxNameFixedOrig :: TestTree
testFormatTabularMaxNameFixedOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) (Just $ ColFormatFixed 50)
    desc = "Tabular max file name and fixed original path"
    fileName = [osp|tabular-max-name-fix-orig|]

testFormatTabularAutoNameFixedOrig :: TestTree
testFormatTabularAutoNameFixedOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff Nothing (Just $ ColFormatFixed 50)
    desc = "Tabular auto file name and fixed original path"
    fileName = [osp|tabular-auto-name-fix-orig|]

mkIndex :: (MonadFail f) => f (Seq PathData)
mkIndex = do
  ts <- ts'
  pure
    [ UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|foo|])
        (MkPathI [osp|/path/foo|])
        (fromℤ 70)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|bazzz|])
        (MkPathI [osp|/path/bar/bazzz|])
        (fromℤ 5_000)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeDirectory)
        (MkPathI [osp|dir|])
        (MkPathI [osp|/some/really/really/long/dir|])
        (fromℤ 20_230)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|f|])
        (MkPathI [osp|/foo/path/f|])
        (fromℤ 13_070_000)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeDirectory)
        (MkPathI [osp|d|])
        (MkPathI [osp|/d|])
        (fromℤ 5_000_000_000_000_000_000_000_000_000)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|z|])
        (MkPathI [osp|/z|])
        (fromℤ 200_120)
        ts
    ]
  where
    -- 5,000 Y
    ts' :: (MonadFail f) => f Timestamp
    ts' = fromText "2020-05-31T12:00:00"

toBS :: Text -> ByteString
toBS = TEnc.encodeUtf8

formatTabularAuto :: PathDataFormat
formatTabularAuto = FormatTabular ColoringOff Nothing Nothing

-- | Golden tests for different combinations of PathDataFormat + Index +
-- Sort + Reverse.
testGoldenFormatParams ::
  -- | Test description
  String ->
  -- | Golden filepath
  OsPath ->
  -- | Style
  PathDataFormat ->
  -- | Column upon which to sort
  Sort ->
  -- | Sort style
  Bool ->
  TestTree
testGoldenFormatParams desc fileName style sortCol rev =
  testGolden desc fileName mkIndex style sortCol rev termWidth
  where
    termWidth = 61

-- Golden tests for different combinations of PathDataFormat + Index +
-- Terminal size.
--
--
testGoldenFormat ::
  -- | Test description
  String ->
  -- | Golden filepath
  OsPath ->
  -- | Action producing the index
  IO (Seq PathData) ->
  -- | Style
  PathDataFormat ->
  -- | Terminal size
  Natural ->
  TestTree
testGoldenFormat desc fileName mkIdx style =
  testGolden desc fileName mkIdx style Name False

-- | General function for running golden tests.
testGolden ::
  -- | Test description
  String ->
  -- | Golden filepath
  OsPath ->
  -- | Action producing the index
  IO (Seq PathData) ->
  -- | Style
  PathDataFormat ->
  -- | Column on which to sort
  Sort ->
  -- | True = asc sort, false = desc
  Bool ->
  -- | Terminal size
  Natural ->
  TestTree
testGolden
  desc
  fileName
  mkIdx
  style
  sortFn
  rev
  termWidth = TestUtils.goldenDiffCustom
    desc
    (unsafeDecode gpath)
    (unsafeDecode apath)
    $ do
      idx <- mkIdx
      let fmt = Index.formatIndex' (MkListCmd style sortFn rev) idx
      formatted <- runConfigIO fmt termWidth
      writeBinaryFile apath (toBS formatted)
    where
      (gpath, apath) = mkGoldenPaths fileName

mkGoldenPaths :: OsPath -> (OsPath, OsPath)
-- NOTE: Using the same goldens for all backends since we want formatting to be
-- consistent.
mkGoldenPaths fp =
  ( goldenPath </> fp <> [osp|.golden|],
    goldenPath </> fp <> [osp|.actual|]
  )
  where
    goldenPath = [osp|test|] </> [osp|unit|] </> [osp|Unit|] </> [osp|Data|] </> [osp|Index|]
