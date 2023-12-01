{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Trash
module Unit.Backend.Default.Trash
  ( tests,
  )
where

import Charon.Backend.Cbor.BackendArgs qualified as Cbor.BackendArgs
import Charon.Backend.Cbor.PathData qualified as Cbor.PathData
import Charon.Backend.Data (Backend (BackendCbor))
import Charon.Backend.Data qualified as Backend
import Charon.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        toCorePathData,
        toPd
      ),
  )
import Charon.Backend.Default.Trash qualified as Trash
import Charon.Backend.Fdo.PathData qualified as Fdo.PathData
import Charon.Backend.Json.BackendArgs qualified as Json.BackendArgs
import Charon.Backend.Json.PathData qualified as Json.PathData
import Charon.Class.Serial (Serial)
import Charon.Data.PathData qualified as PathData
import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import Charon.Data.Timestamp (Timestamp, fromText)
import Charon.Env (HasBackend)
import Charon.Exception (DotsPathE, EmptyPathE, RootE)
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.PathReader (MonadPathReader (pathIsSymbolicLink))
import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))
import Effects.LoggerNS
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNS qualified as Logger
import Numeric.Literal.Integer (FromInteger (afromInteger))
import System.OsPath (encodeUtf)
import System.OsPath qualified as FP
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Unit.Backend.Default.Trash"
    [ backendTests backendCbor,
      backendTests backendFdo,
      backendTests backendJson
    ]
  where
    backendCbor :: BackendArgs PathDataT Cbor.PathData.PathData
    backendCbor = Cbor.BackendArgs.backendArgs

    backendFdo :: BackendArgs PathDataT Fdo.PathData.PathData
    backendFdo =
      MkBackendArgs
        { backend = BackendCbor,
          toPd = Fdo.PathData.toPathData,
          toCorePathData = \_ pd ->
            pure
              $ PathData.UnsafePathData
                { PathData.pathType = MkPathTypeW PathTypeFile,
                  PathData.fileName = pd ^. #fileName,
                  PathData.originalPath = pd ^. #originalPath,
                  PathData.created = pd ^. #created,
                  PathData.size = afromInteger 5
                },
          fromCorePathData = \pd ->
            Fdo.PathData.UnsafePathData
              { Fdo.PathData.fileName = pd ^. #fileName,
                Fdo.PathData.originalPath = pd ^. #originalPath,
                Fdo.PathData.created = pd ^. #created
              }
        }

    backendJson :: BackendArgs PathDataT Json.PathData.PathData
    backendJson = Json.BackendArgs.backendArgs

data TestEnv = MkTestEnv
  { renameResult :: IORef Text,
    backend :: Backend,
    logsRef :: IORef Text,
    logNamespace :: Namespace
  }
  deriving stock (Generic)
  deriving anyclass (HasBackend)

-- NOTE: Real IO because of MonadThrow, etc. Would be nice to mock these and
-- remove IO.
newtype PathDataT a = MkPathDataT (ReaderT TestEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadIORef,
      MonadPosixCompat,
      MonadReader TestEnv,
      MonadThrow,
      MonadTime
    )
    via ReaderT TestEnv IO

runPathDataT :: BackendArgs PathDataT pd -> PathDataT a -> IO (Text, a)
runPathDataT backendArgs (MkPathDataT x) = do
  ref <- newIORef ""
  logsRef <- newIORef ""
  result <- runReaderT x (MkTestEnv ref (backendArgs ^. #backend) logsRef "")
  t <- readIORef ref
  pure (t, result)

runPathDataTLogs :: BackendArgs PathDataT pd -> PathDataT a -> IO (Text, a)
runPathDataTLogs b (MkPathDataT x) = do
  ref <- newIORef ""
  logsRef <- newIORef ""

  result <-
    runReaderT x (MkTestEnv ref (b ^. #backend) logsRef "")
      `catchAny` \ex -> do
        putStrLn "LOGS"
        readIORef logsRef >>= putStrLn . T.unpack
        putStrLn ""
        throwM ex

  t <- readIORef ref
  pure (t, result)

instance MonadLogger PathDataT where
  monadLoggerLog loc _src lvl msg = do
    formatted <- Logger.formatLog (mkFormatter loc) lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (\(MkTestEnv _ _ logsRef _) -> logsRef)
    modifyIORef' logsRef (<> txt)
    where
      mkFormatter l =
        MkLogFormatter
          { newline = True,
            locStrategy = LocStable l,
            timezone = False
          }

instance MonadLoggerNS PathDataT where
  getNamespace = asks (\(MkTestEnv _ _ _ ns) -> ns)
  localNamespace f =
    local (\te@(MkTestEnv _ _ _ ns) -> te {logNamespace = f ns})

-- No real IO!!!
instance MonadPathWriter PathDataT where
  renameFile p1 p2 =
    ask >>= \(MkTestEnv ref _ _ _) ->
      writeIORef ref ("renamed " <> showt p1 <> " to " <> showt p2)

  -- overridden for a better error message
  removeFile p =
    error
      $ mconcat
        [ "removeFile intentionally unimplemented; unit tests should not be ",
          "using it. Attempted delete: '",
          show p,
          "'"
        ]

instance MonadPathReader PathDataT where
  canonicalizePath = pure . ([osp|home|] </>)
  makeAbsolute = pure . ([osp|home|] </>)

  doesPathExist p
    | pName `L.elem` nexists = pure False
    | otherwise =
        error
          $ mconcat
            [ "Path not found: ",
              show p,
              ". Paths we know about: ",
              show nexists
            ]
    where
      pName = FP.takeFileName p
      nexists =
        [ [osp|foo|],
          [osp| |]
        ]

  doesFileExist p
    | p `L.elem` exists = pure True
    | otherwise =
        error
          $ mconcat
            [ "File not found: ",
              show p,
              ". Files we know about: ",
              show exists
            ]
    where
      exists =
        [ [osp|home|] </> [osp|path|] </> [osp|to|] </> [osp|foo|],
          [osp|home|] </> [osp| |]
        ]

  pathIsSymbolicLink _ = pure False
  doesDirectoryExist _ = pure False
  getFileSize _ = pure 0

-- No real IO!!!
instance MonadFileWriter PathDataT where
  writeBinaryFile _ _ = pure ()

-- No real IO!!!
instance MonadTerminal PathDataT where
  putStr = MkPathDataT . putStr

backendTests ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
backendTests b =
  testGroup
    (Backend.backendTestDesc (b ^. #backend))
    [ mvTrash b,
      mvTrashWhitespace b,
      mvTrashRootError b,
      mvTrashEmptyError b,
      mvTrashDotError b,
      mvTrashDotsError b,
      mvTrashDotsEndError b
    ]

mvTrash ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrash backendArgs = testCase "mvOriginalToTrash success" $ do
  (result, _) <- runPathDataTLogs backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts fooPath)
  windowsify "renamed \"home/path/to/foo\" to \"test/unit/.trash/files/foo\"" @=? T.unpack result
  where
    fooPath = MkPathI $ [osp|path|] </> [osp|to|] </> [osp|foo|]

mvTrashWhitespace ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrashWhitespace backendArgs = testCase "mvOriginalToTrash whitespace success" $ do
  (result, _) <- runPathDataTLogs backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts (MkPathI [osp| |]))
  windowsify "renamed \"home/ \" to \"test/unit/.trash/files/ \"" @=? T.unpack result

mvTrashRootError ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrashRootError backendArgs = testCase desc $ do
  eformatted <-
    tryCS @_ @RootE
      $ runPathDataT backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts rootDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete root! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for root original path"

#if WINDOWS
    rootDir = MkPathI [osp|c:\|]
#else
    rootDir = MkPathI [osp|/|]
#endif

mvTrashEmptyError ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrashEmptyError backendArgs = testCase desc $ do
  -- NOTE: Need filepath's encodeUtf here as the quasiquote and our functions
  -- will (correctly) fail on empty paths. This check is out of paranoia, in
  -- case an empty string somehow gets through.
  empty <- encodeUtf ""
  eformatted <-
    tryCS @_ @EmptyPathE
      $ runPathDataT backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts (MkPathI empty))
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the empty path! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for empty original path"

mvTrashDotError ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrashDotError backendArgs = testCase desc $ do
  eformatted <-
    tryCS @_ @DotsPathE
      $ runPathDataT backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts dotDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the special path '.'! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for dot original path"
    dotDir = MkPathI [osp|.|]

mvTrashDotsError ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrashDotsError backendArgs = testCase desc $ do
  eformatted <-
    tryCS @_ @DotsPathE
      $ runPathDataT backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts dotDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the special path '..'! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for dots original path"
    dotDir = MkPathI [osp|..|]

mvTrashDotsEndError ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    Serial pd,
    Show pd
  ) =>
  BackendArgs PathDataT pd ->
  TestTree
mvTrashDotsEndError backendArgs = testCase desc $ do
  eformatted <-
    tryCS @_ @DotsPathE
      $ runPathDataT backendArgs (Trash.mvOriginalToTrash backendArgs trashHome ts dotDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the special path '/path/with/dots/...'! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for dots original path"
    dotDir = MkPathI [osp|/path/with/dots/...|]

trashHome :: PathI TrashHome
trashHome = MkPathI $ [osp|test|] </> [osp|unit|] </> pathDotTrash

ts :: Timestamp
ts = case fromText "2020-05-31T12:00:00" of
  Nothing -> error "[Unit.Data.PathData.ts]: Error creating timestamp"
  Just t -> t

windowsify :: String -> String
#if WINDOWS
windowsify [] = []
windowsify (c:cs)
  -- double slash due to using Show
  | c == '/' = '\\' : '\\' : windowsify cs
  | otherwise = c : windowsify cs
#else
windowsify = id
#endif
