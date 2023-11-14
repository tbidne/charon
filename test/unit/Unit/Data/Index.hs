{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Data.List qualified as L
import Data.Text.Encoding qualified as TEnc
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import Effects.FileSystem.Utils (unsafeDecodeOsToFp, unsafeEncodeFpToOs)
import Effects.System.Terminal
  ( MonadTerminal (getTerminalSize),
    Window (Window, height, width),
  )
import SafeRm.Backend (Backend (BackendCbor, BackendFdo, BackendJson))
import SafeRm.Backend qualified as Backend
import SafeRm.Backend.Cbor.PathData qualified as Cbor
import SafeRm.Backend.Fdo.PathData qualified as Fdo
import SafeRm.Backend.Json.PathData qualified as Json
import SafeRm.Data.Index (Index (MkIndex), Sort (Name, Size))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathData.Formatting
  ( ColFormat (ColFormatFixed, ColFormatMax),
    PathDataFormat (FormatMultiline, FormatTabular),
  )
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Env (HasTrashHome (getTrashHome))
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

-- Tests w/ fixed format lengths, basically verifying the other args like
-- the multiline tests.
tabularFixedTests :: Backend -> TestTree
tabularFixedTests b =
  testGroup
    "Tabular fixed"
    [ testFormatTabularFixed1 b,
      testFormatTabularFixed2 b,
      testFormatTabularFixed3 b,
      testFormatTabularFixed4 b
    ]

testFormatMultiline1 :: Backend -> TestTree
testFormatMultiline1 b = testGoldenFormatParams b "Multiline, name, asc" [osp|multi-name-asc|] FormatMultiline Name False

testFormatMultiline2 :: Backend -> TestTree
testFormatMultiline2 b = testGoldenFormatParams b "Multiline, name, desc" [osp|multi-name-desc|] FormatMultiline Name True

testFormatMultiline3 :: Backend -> TestTree
testFormatMultiline3 b = testGoldenFormatParams b "Multiline, size, asc" [osp|multi-size-asc|] FormatMultiline Size False

testFormatMultiline4 :: Backend -> TestTree
testFormatMultiline4 b = testGoldenFormatParams b "Multiline, size, desc" [osp|multi-size-desc|] FormatMultiline Size True

testFormatTabularFixed1 :: Backend -> TestTree
testFormatTabularFixed1 b = testGoldenFormatParams b "Tabular, name, asc" [osp|tabular-name-asc|] fixedTabularFormat Name False

testFormatTabularFixed2 :: Backend -> TestTree
testFormatTabularFixed2 b = testGoldenFormatParams b "Tabular, name, desc" [osp|tabular-name-desc|] fixedTabularFormat Name True

testFormatTabularFixed3 :: Backend -> TestTree
testFormatTabularFixed3 b = testGoldenFormatParams b "Tabular, size, asc" [osp|tabular-size-asc|] fixedTabularFormat Size False

testFormatTabularFixed4 :: Backend -> TestTree
testFormatTabularFixed4 b = testGoldenFormatParams b "Tabular, size, desc" [osp|tabular-size-desc|] fixedTabularFormat Size True

fixedTabularFormat :: PathDataFormat
fixedTabularFormat = FormatTabular (Just $ ColFormatFixed 10) (Just $ ColFormatFixed 22)

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
      MonadThread,
      MonadThrow
    )
    via ReaderT TestEnv IO

instance MonadPathReader ConfigIO where
  listDirectory _ = pure []
  pathIsSymbolicLink _ = pure False

  doesFileExist = pure . not . (`L.elem` dirs)
  doesDirectoryExist = pure . (`L.elem` dirs)

  getFileSize p
    | p == [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> [osp|foo|] = pure 70
    | p == [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> [osp|bazzz|] = pure 5_000
    | p == [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> [osp|dir|] = pure 20_230
    | p == [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> [osp|f|] = pure 13_070_000
    | p == [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> [osp|z|] = pure 200_120
    | p == [osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> [osp|d|] = pure 5_000_000_000_000_000_000_000_000_000
    | p == ([osp|test|] </> [osp|unit|] </> [osp|index|] </> [osp|trash|] </> pathFiles </> unsafeEncodeFpToOs (L.replicate 50 'b')) = pure 10
  getFileSize p = error $ "getFileSize: " <> show p

dirs :: [OsPath]
dirs =
  (\f -> trashPath </> pathFiles </> f)
    <$> [ [osp|dir|],
          [osp|d|]
        ]

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
testFormatTabularAutoNormal b = testGoldenFormat b desc fileName (mkIndex b) formatTabularAuto 100
  where
    desc = "Auto tabular format"
    fileName = [osp|tabular-auto-normal|]

testFormatTabularAutoMinTermSize :: Backend -> TestTree
testFormatTabularAutoMinTermSize b = testGoldenFormat b desc fileName (mkIndex b) formatTabularAuto 59
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
        $ MkIndex
        $ case b of
          BackendCbor ->
            [ PathData.PathDataCbor $ Cbor.UnsafePathData (MkPathI [osp|foo|]) (MkPathI $ unsafeEncodeFpToOs $ L.replicate 80 'f') ts,
              PathData.PathDataCbor $ Cbor.UnsafePathData (MkPathI $ unsafeEncodeFpToOs $ L.replicate 50 'b') (MkPathI [osp|bar|]) ts
            ]
          BackendFdo ->
            [ PathData.PathDataFdo $ Fdo.UnsafePathData (MkPathI [osp|foo|]) (MkPathI $ unsafeEncodeFpToOs $ L.replicate 80 'f') ts,
              PathData.PathDataFdo $ Fdo.UnsafePathData (MkPathI $ unsafeEncodeFpToOs $ L.replicate 50 'b') (MkPathI [osp|bar|]) ts
            ]
          BackendJson ->
            [ PathData.PathDataJson $ Json.UnsafePathData (MkPathI [osp|foo|]) (MkPathI $ unsafeEncodeFpToOs $ L.replicate 80 'f') ts,
              PathData.PathDataJson $ Json.UnsafePathData (MkPathI $ unsafeEncodeFpToOs $ L.replicate 50 'b') (MkPathI [osp|bar|]) ts
            ]

testFormatTabularAutoEmpty :: Backend -> TestTree
testFormatTabularAutoEmpty b = testGoldenFormat b desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular empty"
    fileName = [osp|tabular-auto-empty|]
    mkIdx = pure $ MkIndex []

formatTabularAutoFail :: TestTree
formatTabularAutoFail = testCase desc $ do
  let idx = MkIndex []
  eformatted <-
    tryAnyCS
      $ runConfigIO
        (Index.formatIndex formatTabularAuto Name False idx)
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
testFormatTabularMaxNameAutoOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig"
    fileName = [osp|tabular-max-name-auto-orig|]

testFormatTabularMaxNameAutoOrigTrunc :: Backend -> TestTree
testFormatTabularMaxNameAutoOrigTrunc b = testGoldenFormat b desc fileName (mkIndex b) fmt 70
  where
    fmt = FormatTabular (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig truncates orig"
    fileName = [osp|tabular-max-name-auto-orig-trunc|]

testFormatTabularAutoNameMaxOrig :: Backend -> TestTree
testFormatTabularAutoNameMaxOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path"
    fileName = [osp|tabular-auto-name-max-orig|]

testFormatTabularAutoNameMaxOrigTrunc :: Backend -> TestTree
testFormatTabularAutoNameMaxOrigTrunc b = testGoldenFormat b desc fileName (mkIndex b) fmt 70
  where
    fmt = FormatTabular Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path truncates name"
    fileName = [osp|tabular-auto-name-max-orig-trunc|]

testFormatTabularMaxNameMaxOrig :: Backend -> TestTree
testFormatTabularMaxNameMaxOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular (Just ColFormatMax) (Just ColFormatMax)
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
testFormatTabularFixedNameMaxOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular (Just $ ColFormatFixed 50) (Just ColFormatMax)
    desc = "Tabular fixed file name and max original path"
    fileName = [osp|tabular-fix-name-max-orig|]

testFormatTabularFixedNameAutoOrig :: Backend -> TestTree
testFormatTabularFixedNameAutoOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular (Just $ ColFormatFixed 50) Nothing
    desc = "Tabular fixed file name and auto original path"
    fileName = [osp|tabular-fix-name-auto-orig|]

testFormatTabularMaxNameFixedOrig :: Backend -> TestTree
testFormatTabularMaxNameFixedOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular (Just ColFormatMax) (Just $ ColFormatFixed 50)
    desc = "Tabular max file name and fixed original path"
    fileName = [osp|tabular-max-name-fix-orig|]

testFormatTabularAutoNameFixedOrig :: Backend -> TestTree
testFormatTabularAutoNameFixedOrig b = testGoldenFormat b desc fileName (mkIndex b) fmt 100
  where
    fmt = FormatTabular Nothing (Just $ ColFormatFixed 50)
    desc = "Tabular auto file name and fixed original path"
    fileName = [osp|tabular-auto-name-fix-orig|]

mkIndex :: (MonadFail f) => Backend -> f Index
mkIndex b = do
  ts <- ts'
  pure
    $ MkIndex
    $ case b of
      BackendCbor ->
        PathData.PathDataCbor
          <$> [ Cbor.UnsafePathData (MkPathI [osp|foo|]) (MkPathI [osp|/path/foo|]) ts,
                Cbor.UnsafePathData (MkPathI [osp|bazzz|]) (MkPathI [osp|/path/bar/bazzz|]) ts,
                Cbor.UnsafePathData (MkPathI [osp|dir|]) (MkPathI [osp|/some/really/really/long/dir|]) ts,
                Cbor.UnsafePathData (MkPathI [osp|f|]) (MkPathI [osp|/foo/path/f|]) ts,
                Cbor.UnsafePathData (MkPathI [osp|d|]) (MkPathI [osp|/d|]) ts,
                Cbor.UnsafePathData (MkPathI [osp|z|]) (MkPathI [osp|/z|]) ts
              ]
      BackendFdo ->
        PathData.PathDataFdo
          <$> [ Fdo.UnsafePathData (MkPathI [osp|foo|]) (MkPathI [osp|/path/foo|]) ts,
                Fdo.UnsafePathData (MkPathI [osp|bazzz|]) (MkPathI [osp|/path/bar/bazzz|]) ts,
                Fdo.UnsafePathData (MkPathI [osp|dir|]) (MkPathI [osp|/some/really/really/long/dir|]) ts,
                Fdo.UnsafePathData (MkPathI [osp|f|]) (MkPathI [osp|/foo/path/f|]) ts,
                Fdo.UnsafePathData (MkPathI [osp|d|]) (MkPathI [osp|/d|]) ts,
                Fdo.UnsafePathData (MkPathI [osp|z|]) (MkPathI [osp|/z|]) ts
              ]
      BackendJson ->
        PathData.PathDataJson
          <$> [ Json.UnsafePathData (MkPathI [osp|foo|]) (MkPathI [osp|/path/foo|]) ts,
                Json.UnsafePathData (MkPathI [osp|bazzz|]) (MkPathI [osp|/path/bar/bazzz|]) ts,
                Json.UnsafePathData (MkPathI [osp|dir|]) (MkPathI [osp|/some/really/really/long/dir|]) ts,
                Json.UnsafePathData (MkPathI [osp|f|]) (MkPathI [osp|/foo/path/f|]) ts,
                Json.UnsafePathData (MkPathI [osp|d|]) (MkPathI [osp|/d|]) ts,
                Json.UnsafePathData (MkPathI [osp|z|]) (MkPathI [osp|/z|]) ts
              ]
  where
    -- 5,000 Y
    ts' :: (MonadFail f) => f Timestamp
    ts' = fromText "2020-05-31T12:00:00"

toBS :: Text -> ByteString
toBS = TEnc.encodeUtf8

formatTabularAuto :: PathDataFormat
formatTabularAuto = FormatTabular Nothing Nothing

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
  testGolden backend desc fileName (mkIndex backend) style sortCol rev 61

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
  IO Index ->
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
  IO Index ->
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
    let fmt = Index.formatIndex style sortFn rev idx
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
    fpBackend = fp <> [osp|-|] <> unsafeEncodeFpToOs (Backend.backendArg b)
    goldenPath = [osp|test|] </> [osp|unit|] </> [osp|Unit|] </> [osp|Data|] </> [osp|Index|]
