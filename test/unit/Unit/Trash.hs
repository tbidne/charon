{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Trash
module Unit.Trash
  ( tests,
  )
where

import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..))
import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))
import Effects.LoggerNS
  ( LocStrategy (LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    Namespace,
  )
import Effects.LoggerNS qualified as Logger
import PathSize (SubPathData (MkSubPathData))
import PathSize.Data.PathData qualified as PathSize.PathData
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Paths (PathI, PathIndex (..), liftPathI')
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Env (HasBackend (..))
import SafeRm.Exception (EmptyPathE, RootE)
import SafeRm.Trash qualified as Trash
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Trash"
    [ backendTests BackendDefault,
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
      writeIORef ref ("renamed " <> T.pack p1 <> " to " <> T.pack p2)

  -- overridden for a better error message
  removeFile p =
    error $
      mconcat
        [ "removeFile intentionally unimplemented; unit tests should not be ",
          "using it. Attempted delete: '",
          p,
          "'"
        ]

instance MonadPathReader PathDataT where
  canonicalizePath = pure . (windowsify "/home/" </>)

  doesPathExist p
    | p `L.elem` nexists = pure False
    | otherwise = error $ "Path: '" <> p <> "'"
    where
      nexists =
        windowsify
          <$> [ "test/unit/.trash/files/foo",
                "test/unit/.trash/files/ "
              ]

  doesFileExist p
    | p `L.elem` exists = pure True
    | otherwise = error p
    where
      exists =
        windowsify
          <$> [ "/home/path/to/foo",
                "/",
                "/home/ "
              ]

instance MonadPathSize PathDataT where
  findLargestPaths _ p = pure (PathSizeSuccess spd)
    where
      spd = MkSubPathData $ NESeq.fromList (pd :| [])
      pd =
        PathSize.PathData.MkPathData
          { PathSize.PathData.path = p,
            PathSize.PathData.size = 0,
            PathSize.PathData.numFiles = 0,
            PathSize.PathData.numDirectories = 0
          }

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
  (result, _) <- runPathDataTLogs b (Trash.mvOriginalToTrash trashHome ts (liftPathI' windowsify "path/to/foo"))
  windowsify "renamed /home/path/to/foo to test/unit/.trash/files/foo" @=? T.unpack result

mvTrashWhitespace :: Backend -> TestTree
mvTrashWhitespace b = testCase "mvOriginalToTrash whitespace success" $ do
  (result, _) <- runPathDataTLogs b (Trash.mvOriginalToTrash trashHome ts " ")
  windowsify "renamed /home/  to test/unit/.trash/files/ " @=? T.unpack result

mvTrashRootError :: Backend -> TestTree
mvTrashRootError b = testCase desc $ do
  eformatted <-
    tryCS @_ @RootE $
      runPathDataT b (Trash.mvOriginalToTrash trashHome ts rootDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete root! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for root original path"
#if WINDOWS
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif

mvTrashEmptyError :: Backend -> TestTree
mvTrashEmptyError b = testCase desc $ do
  eformatted <-
    tryCS @_ @EmptyPathE $
      runPathDataT b (Trash.mvOriginalToTrash trashHome ts "")
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the empty path! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for empty original path"

trashHome :: PathI TrashHome
trashHome = liftPathI' windowsify "test/unit/.trash"

ts :: Timestamp
ts = case fromText "2020-05-31T12:00:00" of
  Nothing -> error "[Unit.Data.PathData.ts]: Error creating timestamp"
  Just t -> t

windowsify :: String -> String
#if WINDOWS
windowsify [] = []
windowsify (c:cs)
  | c == '/' = '\\' : windowsify cs
  | otherwise = c : windowsify cs
#else
windowsify = id
#endif
