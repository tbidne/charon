{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend
import Charon.Data.Index (Index (MkIndex), Sort (Name, Size))
import Charon.Data.Index qualified as Index
import Charon.Data.PathData (PathData (UnsafePathData))
import Charon.Data.PathData.Formatting (ColFormat (ColFormatFixed, ColFormatMax), Coloring (ColoringOff, ColoringOn), PathDataFormat (FormatMultiline, FormatSingleline, FormatTabular), _FormatTabular)
import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import Charon.Data.Timestamp (Timestamp, fromText)
import Charon.Env (HasTrashHome (getTrashHome))
import Charon.Runner.Command.List
import Data.List qualified as L
import Data.Text.Encoding qualified as TEnc
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import Effects.FileSystem.Utils (unsafeDecodeOsToFp, unsafeEncodeFpToOs)
import Effects.System.Terminal
  ( MonadTerminal (getTerminalSize),
    Window (Window, height, width),
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import System.OsPath qualified as FP
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.Index"
    (formattingTests <$> [minBound .. maxBound])

formattingTests :: Backend -> TestTree
formattingTests b =
  testGroup
    ("Formatting " ++ Backend.backendTestDesc b)
    [ multilineTests b,
      singlelineTests b,
      tabularFixedTests b,
      tabularAutoTests b,
      tabularMaxTests b,
      tabularMiscTests b
    ]

multilineTests :: Backend -> TestTree
multilineTests b =
  testGroup
    "Multiline"
    [ testFormatMultiline1 b,
      testFormatMultiline2 b,
      testFormatMultiline3 b,
      testFormatMultiline4 b
    ]

singlelineTests :: Backend -> TestTree
singlelineTests b =
  testGroup
    "Singeline"
    [ testFormatSingleline1 b,
      testFormatSingleline2 b,
      testFormatSingleline3 b,
      testFormatSingleline4 b,
      testFormatSinglelineColor b
    ]

-- Tests w/ fixed format lengths, basically verifying the other args like
-- the multiline tests.
tabularFixedTests :: Backend -> TestTree
tabularFixedTests b =
  testGroup
    "Tabular fixed"
    [ testFormatTabularFixed1 b,
      testFormatTabularFixed2 b,
      testFormatTabularFixed3 b,
      testFormatTabularFixed4 b,
      testFormatTabularFixedColor b
    ]

testFormatMultiline1 :: Backend -> TestTree
testFormatMultiline1 b =
  testGoldenFormatParams
    b
    "Multiline, name, asc"
    [osp|multi-name-asc|]
    FormatMultiline
    Name
    False

testFormatMultiline2 :: Backend -> TestTree
testFormatMultiline2 b =
  testGoldenFormatParams
    b
    "Multiline, name, desc"
    [osp|multi-name-desc|]
    FormatMultiline
    Name
    True

testFormatMultiline3 :: Backend -> TestTree
testFormatMultiline3 b =
  testGoldenFormatParams
    b
    "Multiline, size, asc"
    [osp|multi-size-asc|]
    FormatMultiline
    Size
    False

testFormatMultiline4 :: Backend -> TestTree
testFormatMultiline4 b =
  testGoldenFormatParams
    b
    "Multiline, size, desc"
    [osp|multi-size-desc|]
    FormatMultiline
    Size
    True

testFormatSingleline1 :: Backend -> TestTree
testFormatSingleline1 b =
  testGoldenFormatParams
    b
    "Singleline, name, asc"
    [osp|single-name-asc|]
    (FormatSingleline ColoringOff)
    Name
    False

testFormatSingleline2 :: Backend -> TestTree
testFormatSingleline2 b =
  testGoldenFormatParams
    b
    "Singleline, name, desc"
    [osp|single-name-desc|]
    (FormatSingleline ColoringOff)
    Name
    True

testFormatSingleline3 :: Backend -> TestTree
testFormatSingleline3 b =
  testGoldenFormatParams
    b
    "Singleline, size, asc"
    [osp|single-size-asc|]
    (FormatSingleline ColoringOff)
    Size
    False

testFormatSingleline4 :: Backend -> TestTree
testFormatSingleline4 b =
  testGoldenFormatParams
    b
    "Singleline, size, desc"
    [osp|single-size-desc|]
    (FormatSingleline ColoringOff)
    Size
    True

testFormatSinglelineColor :: Backend -> TestTree
testFormatSinglelineColor b =
  testGoldenFormatParams
    b
    "Singleline, size, desc, color"
    [osp|single-size-desc-color|]
    (FormatSingleline ColoringOn)
    Size
    True

testFormatTabularFixed1 :: Backend -> TestTree
testFormatTabularFixed1 b =
  testGoldenFormatParams
    b
    "Tabular, name, asc"
    [osp|tabular-name-asc|]
    fixedTabularFormat
    Name
    False

testFormatTabularFixed2 :: Backend -> TestTree
testFormatTabularFixed2 b =
  testGoldenFormatParams
    b
    "Tabular, name, desc"
    [osp|tabular-name-desc|]
    fixedTabularFormat
    Name
    True

testFormatTabularFixed3 :: Backend -> TestTree
testFormatTabularFixed3 b =
  testGoldenFormatParams
    b
    "Tabular, size, asc"
    [osp|tabular-size-asc|]
    fixedTabularFormat
    Size
    False

testFormatTabularFixed4 :: Backend -> TestTree
testFormatTabularFixed4 b =
  testGoldenFormatParams
    b
    "Tabular, size, desc"
    [osp|tabular-size-desc|]
    fixedTabularFormat
    Size
    True

testFormatTabularFixedColor :: Backend -> TestTree
testFormatTabularFixedColor b =
  testGoldenFormatParams
    b
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
      | name == unsafeEncodeFpToOs (L.replicate 50 'b') -> pure 10
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
            width = n
          }
  putStr = liftIO . putStr

instance MonadLogger ConfigIO where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNS ConfigIO where
  getNamespace = pure ""
  localNamespace _ m = m

-- Tests tabular automatic formatting i.e. nothing specified
tabularAutoTests :: Backend -> TestTree
tabularAutoTests b =
  testGroup
    "Tabular auto"
    [ testFormatTabularAutoNormal b,
      testFormatTabularAutoMinTermSize b,
      testFormatTabularAutoApprox b,
      testFormatTabularAutoEmpty b,
      formatTabularAutoFail
    ]

testFormatTabularAutoNormal :: Backend -> TestTree
testFormatTabularAutoNormal b = testGoldenFormat b desc fileName mkIndex formatTabularAuto 100
  where
    desc = "Auto tabular format"
    fileName = [osp|tabular-auto-normal|]

testFormatTabularAutoMinTermSize :: Backend -> TestTree
testFormatTabularAutoMinTermSize b = testGoldenFormat b desc fileName mkIndex formatTabularAuto 59
  where
    desc = "Auto tabular formats minimum terminal size"
    fileName = [osp|tabular-auto-min|]

testFormatTabularAutoApprox :: Backend -> TestTree
testFormatTabularAutoApprox b = testGoldenFormat b desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular falls back to estimates for large paths"
    fileName = [osp|tabular-auto-large-approx|]
    mkIdx = do
      ts <- fromText "2020-05-31T12:00:00"
      pure
        [ UnsafePathData
            (MkPathTypeW PathTypeFile)
            (MkPathI [osp|foo|])
            (MkPathI $ unsafeEncodeFpToOs $ L.replicate 80 'f')
            (afromInteger 70)
            ts,
          UnsafePathData
            (MkPathTypeW PathTypeFile)
            (MkPathI $ unsafeEncodeFpToOs $ L.replicate 50 'b')
            (MkPathI [osp|bar|])
            (afromInteger 10)
            ts
        ]

testFormatTabularAutoEmpty :: Backend -> TestTree
testFormatTabularAutoEmpty b = testGoldenFormat b desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular empty"
    fileName = [osp|tabular-auto-empty|]
    mkIdx = pure []

formatTabularAutoFail :: TestTree
formatTabularAutoFail = testCase desc $ do
  let idx = MkIndex []
  eformatted <-
    tryAnyCS
      $ runConfigIO
        (Index.formatIndex (MkListCmd formatTabularAuto Name False) idx)
        53
  case eformatted of
    Right result ->
      assertFailure
        $ "Expected exception, received result: "
        <> show result
    Left ex ->
      assertBool (displayException ex) (expected `L.isPrefixOf` displayException ex)
  where
    desc = "Auto tabular throws error for small terminal width"
    expected =
      mconcat
        [ "Control.Exception.Safe.throwString called with:\n\nTerminal width (53)",
          " is less than minimum width (54) for automatic tabular display. ",
          "Perhaps try multiline."
        ]

-- Tests tabular w/ max options
tabularMaxTests :: Backend -> TestTree
tabularMaxTests b =
  testGroup
    "Tabular max"
    [ testFormatTabularMaxNameAutoOrig b,
      testFormatTabularMaxNameAutoOrigTrunc b,
      testFormatTabularAutoNameMaxOrig b,
      testFormatTabularAutoNameMaxOrigTrunc b,
      testFormatTabularMaxNameMaxOrig b
    ]

testFormatTabularMaxNameAutoOrig :: Backend -> TestTree
testFormatTabularMaxNameAutoOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig"
    fileName = [osp|tabular-max-name-auto-orig|]

testFormatTabularMaxNameAutoOrigTrunc :: Backend -> TestTree
testFormatTabularMaxNameAutoOrigTrunc b = testGoldenFormat b desc fileName mkIndex fmt 70
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig truncates orig"
    fileName = [osp|tabular-max-name-auto-orig-trunc|]

testFormatTabularAutoNameMaxOrig :: Backend -> TestTree
testFormatTabularAutoNameMaxOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path"
    fileName = [osp|tabular-auto-name-max-orig|]

testFormatTabularAutoNameMaxOrigTrunc :: Backend -> TestTree
testFormatTabularAutoNameMaxOrigTrunc b = testGoldenFormat b desc fileName mkIndex fmt 70
  where
    fmt = FormatTabular ColoringOff Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path truncates name"
    fileName = [osp|tabular-auto-name-max-orig-trunc|]

testFormatTabularMaxNameMaxOrig :: Backend -> TestTree
testFormatTabularMaxNameMaxOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) (Just ColFormatMax)
    desc = "Tabular max file name and max original path"
    fileName = [osp|tabular-max-name-max-orig|]

-- Misc tabular tests e.g. interactions between various options
tabularMiscTests :: Backend -> TestTree
tabularMiscTests b =
  testGroup
    "Tabular misc"
    [ testFormatTabularFixedNameMaxOrig b,
      testFormatTabularFixedNameAutoOrig b,
      testFormatTabularMaxNameFixedOrig b,
      testFormatTabularAutoNameFixedOrig b
    ]

testFormatTabularFixedNameMaxOrig :: Backend -> TestTree
testFormatTabularFixedNameMaxOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just $ ColFormatFixed 50) (Just ColFormatMax)
    desc = "Tabular fixed file name and max original path"
    fileName = [osp|tabular-fix-name-max-orig|]

testFormatTabularFixedNameAutoOrig :: Backend -> TestTree
testFormatTabularFixedNameAutoOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just $ ColFormatFixed 50) Nothing
    desc = "Tabular fixed file name and auto original path"
    fileName = [osp|tabular-fix-name-auto-orig|]

testFormatTabularMaxNameFixedOrig :: Backend -> TestTree
testFormatTabularMaxNameFixedOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular ColoringOff (Just ColFormatMax) (Just $ ColFormatFixed 50)
    desc = "Tabular max file name and fixed original path"
    fileName = [osp|tabular-max-name-fix-orig|]

testFormatTabularAutoNameFixedOrig :: Backend -> TestTree
testFormatTabularAutoNameFixedOrig b = testGoldenFormat b desc fileName mkIndex fmt 100
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
        (afromInteger 70)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|bazzz|])
        (MkPathI [osp|/path/bar/bazzz|])
        (afromInteger 5_000)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeDirectory)
        (MkPathI [osp|dir|])
        (MkPathI [osp|/some/really/really/long/dir|])
        (afromInteger 20_230)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|f|])
        (MkPathI [osp|/foo/path/f|])
        (afromInteger 13_070_000)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeDirectory)
        (MkPathI [osp|d|])
        (MkPathI [osp|/d|])
        (afromInteger 5_000_000_000_000_000_000_000_000_000)
        ts,
      UnsafePathData
        (MkPathTypeW PathTypeFile)
        (MkPathI [osp|z|])
        (MkPathI [osp|/z|])
        (afromInteger 200_120)
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
  -- | Backend
  Backend ->
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
testGoldenFormatParams backend desc fileName style sortCol rev =
  testGolden backend desc fileName mkIndex style sortCol rev termWidth
  where
    termWidth = 61

-- Golden tests for different combinations of PathDataFormat + Index +
-- Terminal size.
--
--
testGoldenFormat ::
  -- | Backend
  Backend ->
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
testGoldenFormat backend desc fileName mkIdx style =
  testGolden backend desc fileName mkIdx style Name False

-- | General function for running golden tests.
testGolden ::
  -- | Backend to use
  Backend ->
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
  backend
  desc
  fileName
  mkIdx
  style
  sortFn
  rev
  termWidth = goldenVsFile desc (unsafeDecodeOsToFp gpath) (unsafeDecodeOsToFp apath) $ do
    idx <- mkIdx
    let fmt = Index.formatIndex' (MkListCmd style sortFn rev) idx
    formatted <- runConfigIO fmt termWidth
    writeBinaryFile apath (toBS formatted)
    where
      (gpath, apath) = mkGoldenPaths backend fileName

mkGoldenPaths :: Backend -> OsPath -> (OsPath, OsPath)
-- NOTE: Using the same goldens for all backends since we want formatting to be
-- consistent.
mkGoldenPaths b fp =
  ( goldenPath </> fpBackend <> [osp|.golden|],
    goldenPath </> fpBackend <> [osp|.actual|]
  )
  where
    fpBackend = fp <> [osp|-|] <> unsafeEncodeFpToOs (Backend.backendName b)
    goldenPath = [osp|test|] </> [osp|unit|] </> [osp|Unit|] </> [osp|Data|] </> [osp|Index|]
