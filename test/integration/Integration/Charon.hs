{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Property tests for the Charon API.
module Integration.Charon
  ( tests,
  )
where

import Charon qualified
import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend.Data
import Charon.Data.PathData (PathData)
import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.PathType qualified as PathType
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath
      ),
  )
import Charon.Data.UniqueSeqNE (UniqueSeqNE, (↦), (∪))
import Charon.Env (HasBackend, HasTrashHome (getTrashHome))
import Charon.Env qualified as Env
import Charon.Exception (PathNotFound, TrashEntryNotFoundE)
import Charon.Runner.CharonT (CharonT (MkCharonT))
import Charon.Runner.Command
  ( DeleteParams (MkDeleteParams),
    Force (MkForce),
    IndicesPathsStrategy (PathsStrategy),
    NoPrompt (MkNoPrompt),
    PermDeleteParams (MkPermDeleteParams),
    RestoreParams (MkRestoreParams),
  )
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LogEnv (MkLogEnv),
  )
import Charon.Runner.Env
  ( Env (MkEnv, coreConfig),
  )
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.Logger.Namespace (defaultLogFormatter)
import Effects.Logger.Namespace qualified as Logger
import GHC.IsList (IsList (toList))
import Hedgehog (PropertyT)
import Integration.AsciiOnly (AsciiOnly (MkAsciiOnly))
import Integration.Prelude
import Integration.Utils (PathIntData, PathWithType)
import Integration.Utils qualified as IntUtils
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet), withGuard_)

-- Custom type for running the tests. Fo now, the only reason we do not use
-- CharonT is to override getFileSize so that expected errors in tests
-- do not spam the console logs (i.e. retrieving the size for a bad path).
-- We could use this to later verify logs, if we wished.

data IntEnv = MkIntEnv
  { coreEnv :: Env,
    termLogsRef :: IORef [Text],
    logsRef :: IORef [Text],
    namespace :: Namespace
  }

makeFieldLabelsNoPrefix ''IntEnv

instance HasBackend IntEnv where
  getBackend = view (#coreEnv % #coreConfig % #backend)

instance HasTrashHome IntEnv where
  getTrashHome = view (#coreEnv % #coreConfig % #trashHome)

-- | Type for running integration tests.
newtype IntIO a = MkIntIO (ReaderT IntEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadIO,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIORef,
      MonadPathReader,
      MonadPathWriter,
      MonadPosixCompat,
      MonadReader IntEnv,
      MonadThread,
      MonadThrow,
      MonadTime
    )
    via (CharonT IntEnv IO)

#if !WINDOWS
deriving newtype instance MonadPosix IntIO
#endif

instance MonadLogger IntIO where
  monadLoggerLog loc _ lvl msg = do
    formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (view #logsRef)
    modifyIORef' logsRef (txt :)

-- Wno-missing-methods
instance MonadTerminal IntIO where
  putStr s =
    asks (view #termLogsRef)
      >>= \ref -> modifyIORef' ref ((:) . T.pack $ s)

instance MonadHaskeline IntIO

-- | Default way to run integration tests. Ensures uncaught exceptions do not
-- mess with annotations
usingIntIO :: IntEnv -> IntIO a -> PropertyT IO a
usingIntIO env rdr = do
  -- NOTE: Evidently annotations do not work with uncaught exceptions.
  -- Therefore we catch any exceptions here, print out any relevant data,
  -- then use Hedgehog's failure to die.
  result <-
    usingIntIONoCatch env rdr `catchSync` \ex -> do
      printLogs
      annotate $ displayException ex
      failure

  -- for help debugging CI failures
  withGuard_ "INT_LOGS" ExpectEnvSet printLogs

  pure result
  where
    printLogs = do
      termLogs <- liftIO $ readIORef (env ^. #termLogsRef)
      logs <- liftIO $ readIORef (env ^. #logsRef)
      annotate "TERMINAL LOGS"
      for_ termLogs (annotate . T.unpack)
      annotate "FILE LOGS"
      for_ (L.reverse logs) (annotate . T.unpack)

-- | Use this for when we are expecting an exception and want to specifically
-- handle it in the test
usingIntIONoCatch :: IntEnv -> IntIO a -> PropertyT IO a
usingIntIONoCatch env (MkIntIO rdr) = liftIO $ runReaderT rdr env

tests :: IO OsPath -> TestTree
tests testDir =
  testGroup
    "Charon"
    (testsBackend testDir <$> [minBound .. maxBound])

testsBackend :: IO OsPath -> Backend -> TestTree
testsBackend testDir b =
  testGroup
    (Backend.Data.backendTestDesc b)
    [ delete b testDir,
      deleteSome b testDir,
      permDelete b testDir,
      permDeleteSome b testDir,
      restore b testDir,
      restoreSome b testDir,
      emptyTrash b testDir,
      metadata b testDir
    ]

delete :: Backend -> IO OsPath -> TestTree
delete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "All paths are deleted" "delete" $ do
    property $ do
      annotateShow b
      testDir <- getTestPath mtestDir [osp|delete|] backend
      α <- forAll (IntUtils.genFileNameSet b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ charonDelete (αTestPaths ↦ MkPathI)

      annotate "Assert lookup"

      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- get index
      index <- getIndex env
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      αTestPaths ^. #set === indexOrigPaths

deleteSome :: Backend -> IO OsPath -> TestTree
deleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|deleteSome|] backend
      (α, β) <- forAll (IntUtils.gen2FileNameSets b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          (_, _, βTestPaths) = mkPaths testDir β

          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      -- should succeed on α and fail on β
      let toDelete = αTestPaths ∪ βTestPaths

      caughtEx <-
        try @_ @PathNotFound
          $ usingIntIONoCatch env (charonDelete (toDelete ↦ MkPathI))

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- should have printed exactly 1 message, since we fail fast
      termLogs <- liftIO $ readIORef (env ^. #termLogsRef)
      length termLogs === 1

      -- assert original files moved to trash
      annotate "Assert lookup"

      -- get index
      index <- getIndex env
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- index should exactly match α
      αTestPaths ^. #set === indexOrigPaths

permDelete :: Backend -> IO OsPath -> TestTree
permDelete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDelete" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|permDelete|] backend
      α <- forAll (IntUtils.genFileNameSet b)
      let trashDir = testDir </> [osp|.trash|]
          (αNames, αTest, αTestPaths) = mkPaths testDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ charonDelete (αTestPaths ↦ MkPathI)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- permanently delete files
      let toPermDelete = αNames ↦ MkPathI
      usingIntIO env $ charonPermDelete toPermDelete

      -- get index
      index <- getIndex env
      annotateShow index

      [] === index
      assertPathsDoNotExist αTestPaths
  where
    desc = "All trash entries are permanently deleted"

permDeleteSome :: Backend -> IO OsPath -> TestTree
permDeleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDeleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|permDeleteSome|] backend
      (α, β, γ) <- forAll (IntUtils.gen3FileNameSets b)
      let (αNames, αTest, αTestPaths) = mkPaths testDir α
          (βNames, _, _) = mkPaths testDir β
          (_, γTest, γTestPaths) = mkPaths testDir γ

          toDelete = αTestPaths ∪ γTestPaths
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      setupDir testDir (αTest ∪ γTest)

      -- delete files
      usingIntIO env $ charonDelete (toDelete ↦ MkPathI)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- permanently delete files
      -- should succeed on α and fail on β
      let toPermDelete = αNames ∪ βNames ↦ MkPathI
      annotateShow toPermDelete

      caughtEx <-
        try @_ @TrashEntryNotFoundE
          $ usingIntIONoCatch env (charonPermDelete toPermDelete)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- getIndex env
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      γTestPaths ^. #set === indexOrigPaths

      annotate "Assert files exist"
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: Backend -> IO OsPath -> TestTree
restore backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Restores all trash entries" "restore" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|restore|] backend
      α <- forAll (IntUtils.genFileNameSet b)
      let (αNames, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ charonDelete (αTestPaths ↦ MkPathI)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- restore files
      let toRestore = αNames ↦ MkPathI
      usingIntIO env $ charonRestore toRestore

      -- get index
      index <- getIndex env
      annotateShow index

      [] === index
      annotate "Assert files exist"
      assertPathsExist αTestPaths

restoreSome :: Backend -> IO OsPath -> TestTree
restoreSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "restoreSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|restoreSome|] backend
      (α, β, γ) <- forAll (IntUtils.gen3FileNameSets b)
      let (αNames, αTest, αTestPaths) = mkPaths testDir α
          (βNames, _, _) = mkPaths testDir β
          (_, γTest, γTestPaths) = mkPaths testDir γ

          toDelete = αTestPaths ∪ γTestPaths
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      setupDir testDir (αTest ∪ γTest)

      -- delete files
      usingIntIO env $ charonDelete (toDelete ↦ MkPathI)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- restore
      -- should succeed on α and fail on β
      let toRestore = αNames ∪ βNames ↦ MkPathI
      annotateShow toRestore

      caughtEx <-
        try @_ @TrashEntryNotFoundE
          $ usingIntIONoCatch env (charonRestore toRestore)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- getIndex env
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      γTestPaths ^. #set === indexOrigPaths

      annotate "Assert files exist"
      assertPathsExist αTestPaths
      annotate "Assert lookup"
  where
    desc = "Some trash entries are restored, others error"

charonDelete :: UniqueSeqNE (PathI TrashEntryOriginalPath) -> IntIO ()
charonDelete =
  Charon.delete
    . MkDeleteParams

charonPermDelete :: UniqueSeqNE (PathI TrashEntryFileName) -> IntIO ()
charonPermDelete =
  Charon.permDelete
    . MkPermDeleteParams (MkNoPrompt True)
    . PathsStrategy

charonRestore :: UniqueSeqNE (PathI TrashEntryFileName) -> IntIO ()
charonRestore =
  Charon.restore
    . MkRestoreParams (MkForce False) (MkNoPrompt True)
    . PathsStrategy

emptyTrash :: Backend -> IO OsPath -> TestTree
emptyTrash backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Empties the trash" "empty" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|emptyTrash|] backend
      α <- forAll (IntUtils.genFileNameSet b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ charonDelete (αTestPaths ↦ MkPathI)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- empty trash
      usingIntIO env $ Charon.emptyTrash (MkNoPrompt True)

      -- get index
      index <- getIndex env
      annotateShow index

      [] === index
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

metadata :: Backend -> IO OsPath -> TestTree
metadata backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Retrieves metadata" "metadata" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|metadata|] backend
      α <- forAll (IntUtils.genFileNameSet b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ charonDelete (αTestPaths ↦ MkPathI)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- get metadata
      metadata' <- usingIntIO env Charon.getMetadata
      annotateShow metadata'

      length α === natToInt (metadata' ^. #numEntries)

      countFiles αTest === natToInt (metadata' ^. #numFiles)

natToInt :: (HasCallStack) => Natural -> Int
natToInt i
  | i <= intMax = fromIntegral i
  | otherwise = error $ "natToInt: Too large to convert to Int: " <> show i
  where
    intMax = fromIntegral (maxBound :: Int)

toOrigPath :: HashSet OsPath -> PathData -> HashSet OsPath
toOrigPath acc pd = HSet.insert (pd ^. #originalPath % #unPathI) acc

mkEnv :: Backend -> OsPath -> IO IntEnv
mkEnv backend fp = do
  termLogsRef <- newIORef []
  logsRef <- newIORef []

  pure
    $ MkIntEnv
      { coreEnv =
          MkEnv
            { coreConfig =
                MkCoreConfig
                  { trashHome = MkPathI fp,
                    backend,
                    logging = MkLogEnv Nothing ""
                  }
            },
        termLogsRef,
        logsRef,
        namespace = "integration"
      }

-- NOTE: [OSX temp symlink]
--
-- This exists because in the osx tests', canonicalize transforms a path
--
--    /var/...
--
-- into
--
--    /private/var/...
--
-- due to some symlink shenanigans. This causes tests to fail because the
-- trash dir we create looks like /var/... whereas the PathData's
-- originalPath is /private/var/...
--
-- Thus we canonicalize the test path as well, allowing our
-- "deleted paths match expected paths" checks to succeed.
--
-- This is not needed on linux but also appears unharmful.
getTestPath :: (MonadIO m) => IO OsPath -> OsPath -> Backend -> m OsPath
getTestPath mtestPath p backend = do
  testPath <- liftIO $ makeAbsolute =<< mtestPath
  pure $ testPath </> p </> Backend.Data.backendArgOsPath backend

setupDir ::
  ( MonadCatch m,
    MonadIO m,
    MonadTest m
  ) =>
  OsPath ->
  UniqueSeqNE PathWithType ->
  m ()
setupDir dir paths = do
  let action = do
        clearDirectory dir

        -- see NOTE: [Duplicate test files]
        for_ paths $ \(p, MkPathTypeW ty) -> case ty of
          PathTypeFile -> createFileContents (p, "")
          PathTypeDirectory -> createDirectory p
          PathTypeSymbolicLink -> createSymlink (F p)
          PathTypeOther -> createFileContents (p, "")
        assertPathsExist (fmap (view _1) paths')

  action `catchSync` \ex -> do
    annotate $ displayException ex
    failure
  where
    paths' :: [PathWithType]
    paths' = toList $ NESeq.toSeq $ view #seq paths

-- | Given a test dir and a set of names, forms the paths needed to run
-- tests.
mkPaths ::
  -- | TestDir
  OsPath ->
  -- | Set of pathNames
  UniqueSeqNE PathIntData ->
  -- | (pathNames, (testDir </> pathNames, a), testDir </> pathNames)
  ( UniqueSeqNE OsPath,
    UniqueSeqNE PathWithType,
    UniqueSeqNE OsPath
  )
mkPaths testDir paths =
  ( paths ↦ view #osPath,
    prefixed,
    prefixed ↦ view _1
  )
  where
    prefixed = paths ↦ (\x -> (testDir </> view #osPath x, view #pathType x))

countFiles :: UniqueSeqNE PathWithType -> Int
countFiles = length . filter isNotDir . toList . NESeq.toSeq . view #seq
  where
    isNotDir :: PathWithType -> Bool
    isNotDir (_, pt) = not $ PathType.isDirectory pt

getIndex :: IntEnv -> PropertyT IO (Seq PathData)
getIndex env = fmap (view _1) . view #unIndex <$> usingIntIO env Charon.getIndex
