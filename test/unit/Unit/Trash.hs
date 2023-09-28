{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for Trash
module Unit.Trash
  ( tests,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Effectful.FileSystem.FileWriter.Dynamic
  ( FileWriterDynamic (WriteBinaryFile),
  )
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic (CanonicalizePath, DoesFileExist, DoesPathExist),
  )
import Effectful.FileSystem.PathWriter.Dynamic
  ( PathWriterDynamic
      ( RemoveFile,
        RenameFile
      ),
  )
import Effectful.Logger.Dynamic (LoggerDynamic (LoggerLog))
import Effectful.LoggerNS.Dynamic
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    LoggerNSDynamic (GetNamespace, LocalNamespace),
  )
import Effectful.LoggerNS.Dynamic qualified as Logger
import Effectful.Time.Dynamic (runTimeDynamicIO)
import SafeRm.Data.Backend (Backend (BackendCbor, BackendFdo))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Env (HasBackend)
import SafeRm.Exception (EmptyPathE, RootE)
import SafeRm.Trash qualified as Trash
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Trash"
    [ backendTests BackendCbor,
      backendTests BackendFdo
    ]

data TestEnv = MkTestEnv
  { renameResult :: !(IORef Text),
    backend :: !Backend,
    logsRef :: !(IORef Text),
    logNamespace :: !Namespace
  }
  deriving stock (Generic)
  deriving anyclass (HasBackend)

runPathDataHelper ::
  IORef Text ->
  IORef Text ->
  Backend ->
  Eff
    [ PathReaderDynamic,
      PathWriterDynamic,
      FileWriterDynamic,
      LoggerDynamic,
      LoggerNSDynamic,
      TimeDynamic,
      IORefStatic,
      Reader TestEnv,
      IOE
    ]
    a ->
  IO a
runPathDataHelper ref logsRef b = run
  where
    run =
      runEff
        . runReader (MkTestEnv ref b logsRef "")
        . runIORefStaticIO
        . runTimeDynamicIO
        . runLoggerNS
        . runLogger
        . runFileWriter
        . runPathWriter
        . runPathReader

runPathDataT ::
  Backend ->
  Eff
    [ PathReaderDynamic,
      PathWriterDynamic,
      FileWriterDynamic,
      LoggerDynamic,
      LoggerNSDynamic,
      TimeDynamic,
      IORefStatic,
      Reader TestEnv,
      IOE
    ]
    a ->
  IO (Text, a)
runPathDataT b x = do
  ref <- newIORef ""
  logsRef <- newIORef ""
  result <- runPathDataHelper ref logsRef b x
  t <- readIORef ref
  pure (t, result)

runPathDataTLogs ::
  Backend ->
  Eff
    [ PathReaderDynamic,
      PathWriterDynamic,
      FileWriterDynamic,
      LoggerDynamic,
      LoggerNSDynamic,
      TimeDynamic,
      IORefStatic,
      Reader TestEnv,
      IOE
    ]
    a ->
  IO (Text, a)
runPathDataTLogs b x = do
  ref <- newIORef ""
  logsRef <- newIORef ""

  result <-
    runPathDataHelper ref logsRef b x
      `catchAny` \ex -> do
        putStrLn "LOGS"
        readIORef logsRef >>= putStrLn . T.unpack
        putStrLn ""
        throwM ex

  t <- readIORef ref
  pure (t, result)

runLogger ::
  forall es a.
  ( IORefStatic :> es,
    LoggerNSDynamic :> es,
    Reader TestEnv :> es,
    TimeDynamic :> es
  ) =>
  Eff (LoggerDynamic : es) a ->
  Eff es a
runLogger = interpret $ \_ -> \case
  LoggerLog loc _src lvl msg -> do
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

runLoggerNS ::
  ( Reader TestEnv :> es
  ) =>
  Eff (LoggerNSDynamic : es) a ->
  Eff es a
runLoggerNS = interpret $ \env -> \case
  GetNamespace -> asks (\(MkTestEnv _ _ _ ns) -> ns)
  LocalNamespace f m -> localSeqUnlift env $ \runner ->
    local (\te@(MkTestEnv _ _ _ ns) -> te {logNamespace = f ns}) (runner m)

-- No real IO!!!
runPathWriter ::
  ( IORefStatic :> es,
    Reader TestEnv :> es
  ) =>
  Eff (PathWriterDynamic : es) a ->
  Eff es a
runPathWriter = interpret $ \_ -> \case
  RenameFile p1 p2 ->
    ask >>= \(MkTestEnv ref _ _ _) ->
      writeIORef ref ("renamed " <> showt p1 <> " to " <> showt p2)
  -- overridden for a better error message
  RemoveFile p ->
    error
      $ mconcat
        [ "removeFile intentionally unimplemented; unit tests should not be ",
          "using it. Attempted delete: '",
          show p,
          "'"
        ]
  _ -> error "Unit.Trash.PathWriter: unimplemented"

runPathReader ::
  Eff (PathReaderDynamic : es) a ->
  Eff es a
runPathReader = interpret $ \_ -> \case
  CanonicalizePath p -> pure $ [osp|home|] </> p
  DoesPathExist p
    | p `L.elem` nexists -> pure False
    | otherwise ->
        error
          $ mconcat
            [ "Path not found: ",
              show p,
              ". Paths we know about: ",
              show nexists
            ]
    where
      nexists =
        [ pathTest </> pathUnit </> pathDotTrash </> pathFiles </> [osp|foo|],
          pathTest </> pathUnit </> pathDotTrash </> pathFiles </> [osp| |]
        ]
  DoesFileExist p
    | p `L.elem` exists -> pure True
    | otherwise ->
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
  _ -> error "Unit.Trash.PathReader: unimplemented"

-- No real IO!!!
runFileWriter ::
  Eff (FileWriterDynamic : es) a ->
  Eff es a
runFileWriter = interpret $ \_ -> \case
  WriteBinaryFile _ _ -> pure ()
  _ -> error "Unit.Trash.FileWriter: unimplemented"

backendTests :: Backend -> TestTree
backendTests b =
  testGroup
    (Backend.backendTestDesc b)
    [ mvTrash b,
      mvTrashWhitespace b,
      mvTrashRootError b,
      mvTrashEmptyError b
    ]

mvTrash :: Backend -> TestTree
mvTrash b = testCase "mvOriginalToTrash success" $ do
  (result, _) <- runPathDataTLogs b (Trash.mvOriginalToTrash @TestEnv trashHome ts fooPath)
  windowsify "renamed \"home/path/to/foo\" to \"test/unit/.trash/files/foo\"" @=? T.unpack result
  where
    fooPath = MkPathI $ [osp|path|] </> [osp|to|] </> [osp|foo|]

mvTrashWhitespace :: Backend -> TestTree
mvTrashWhitespace b = testCase "mvOriginalToTrash whitespace success" $ do
  (result, _) <- runPathDataTLogs b (Trash.mvOriginalToTrash @TestEnv trashHome ts (MkPathI [osp| |]))
  windowsify "renamed \"home/ \" to \"test/unit/.trash/files/ \"" @=? T.unpack result

mvTrashRootError :: Backend -> TestTree
mvTrashRootError b = testCase desc $ do
  eformatted <-
    try @_ @RootE
      $ runPathDataT b (Trash.mvOriginalToTrash @TestEnv trashHome ts rootDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete root! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for root original path"
#if WINDOWS
    rootDir = MkPathI [osp|C:\|]
#else
    rootDir = MkPathI [osp|/|]
#endif

mvTrashEmptyError :: Backend -> TestTree
mvTrashEmptyError b = testCase desc $ do
  -- NOTE: Need unsafeEncodeFpToOs here as the quasiquote and our functions
  -- will (correctly) fail on empty paths. This check is out of paranoia, in
  -- case an empty string somehow gets through.
  empty <- encodeFpToOsThrowM ""
  eformatted <-
    try @_ @EmptyPathE
      $ runPathDataT b (Trash.mvOriginalToTrash @TestEnv trashHome ts (MkPathI empty))
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the empty path! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for empty original path"

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

pathTest :: OsPath
pathTest = [osp|test|]

pathUnit :: OsPath
pathUnit = [osp|unit|]
