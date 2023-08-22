{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Trash
module Unit.Trash
  ( tests,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))
import Effects.LoggerNS
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNS qualified as Logger
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Env (HasBackend (..))
import SafeRm.Exception (EmptyPathE, RootE)
import SafeRm.Trash qualified as Trash
import System.OsPath (encodeUtf)
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

-- NOTE: Real IO because of MonadThrow, etc. Would be nice to mock these and
-- remove IO.
newtype PathDataT a = MkPathDataT (ReaderT TestEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadIORef,
      MonadReader TestEnv,
      MonadThrow,
      MonadTime
    )
    via ReaderT TestEnv IO

runPathDataT :: Backend -> PathDataT a -> IO (Text, a)
runPathDataT b (MkPathDataT x) = do
  ref <- newIORef ""
  logsRef <- newIORef ""
  result <- runReaderT x (MkTestEnv ref b logsRef "")
  t <- readIORef ref
  pure (t, result)

runPathDataTLogs :: Backend -> PathDataT a -> IO (Text, a)
runPathDataTLogs b (MkPathDataT x) = do
  ref <- newIORef ""
  logsRef <- newIORef ""

  result <-
    runReaderT x (MkTestEnv ref b logsRef "")
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

  doesPathExist p
    | p `L.elem` nexists = pure False
    | otherwise =
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

-- No real IO!!!
instance MonadFileWriter PathDataT where
  writeBinaryFile _ _ = pure ()

-- No real IO!!!
instance MonadTerminal PathDataT where
  putStr = MkPathDataT . putStr

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
  (result, _) <- runPathDataTLogs b (Trash.mvOriginalToTrash trashHome ts fooPath)
  windowsify "renamed \"home/path/to/foo\" to \"test/unit/.trash/files/foo\"" @=? T.unpack result
  where
    fooPath = MkPathI $ [osp|path|] </> [osp|to|] </> [osp|foo|]

mvTrashWhitespace :: Backend -> TestTree
mvTrashWhitespace b = testCase "mvOriginalToTrash whitespace success" $ do
  (result, _) <- runPathDataTLogs b (Trash.mvOriginalToTrash trashHome ts (MkPathI [osp| |]))
  windowsify "renamed \"home/ \" to \"test/unit/.trash/files/ \"" @=? T.unpack result

mvTrashRootError :: Backend -> TestTree
mvTrashRootError b = testCase desc $ do
  eformatted <-
    tryCS @_ @RootE
      $ runPathDataT b (Trash.mvOriginalToTrash trashHome ts rootDir)
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
  -- NOTE: Need filepath's encodeUtf here as the quasiquote and our functions
  -- will (correctly) fail on empty paths. This check is out of paranoia, in
  -- case an empty string somehow gets through.
  empty <- encodeUtf ""
  eformatted <-
    tryCS @_ @EmptyPathE
      $ runPathDataT b (Trash.mvOriginalToTrash trashHome ts (MkPathI empty))
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
