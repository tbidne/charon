{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Property tests for the SafeRm API.
module Integration.SafeRm
  ( tests,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Char qualified as Ch
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.LoggerNS (Namespace, defaultLogFormatter)
import Effects.LoggerNS qualified as Logger
import GHC.Exts (IsList (Item, fromList))
import Hedgehog (PropertyT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integration.AsciiOnly (AsciiOnly (..))
import Integration.Prelude
import PathSize qualified
import SafeRm qualified
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Data.UniqueSeq qualified as USeq
import SafeRm.Env (HasBackend (..), HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Exception (FileNotFoundE, TrashEntryNotFoundE)
import SafeRm.Runner.Env
  ( Env (..),
    LogEnv (MkLogEnv),
  )
import SafeRm.Runner.SafeRmT (SafeRmT (..))
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet), withGuard_)

-- Custom type for running the tests. Fo now, the only reason we do not use
-- SafeRmT is to override getFileSize so that expected errors in tests
-- do not spam the console logs (i.e. retrieving the size for a bad path).
-- We could use this to later verify logs, if we wished.

data IntEnv = MkIntEnv
  { coreEnv :: Env IO,
    termLogsRef :: IORef [Text],
    logsRef :: IORef [Text],
    namespace :: !Namespace
  }

makeFieldLabelsNoPrefix ''IntEnv

instance HasBackend IntEnv where
  getBackend = view (#coreEnv % #backend)

instance HasTrashHome IntEnv where
  getTrashHome = view (#coreEnv % #trashHome)

-- | Type for running integration tests.
newtype IntIO a = MkIntIO (ReaderT IntEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIORef,
      MonadPathReader,
      MonadPathWriter,
      MonadReader IntEnv,
      MonadThrow,
      MonadTime
    )
    via (SafeRmT IntEnv IO)

instance MonadPathSize IntIO where
  findLargestPaths _ _ =
    pure $
      PathSize.PathSizeSuccess $
        PathSize.MkSubPathData $
          NESeq.singleton $
            PathSize.MkPathData
              { PathSize.path = "",
                PathSize.size = 5,
                PathSize.numFiles = 10,
                PathSize.numDirectories = 20
              }

instance MonadLogger IntIO where
  monadLoggerLog loc _ lvl msg = do
    formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (view #logsRef)
    modifyIORef' logsRef (txt :)

instance MonadLoggerNS IntIO where
  getNamespace = view #namespace <$> ask
  localNamespace f = local (over' #namespace f)

-- Wno-missing-methods
instance MonadTerminal IntIO where
  putStr s =
    asks (view #termLogsRef)
      >>= \ref -> modifyIORef' ref ((:) . T.pack $ s)

-- | Default way to run integration tests. Ensures uncaught exceptions do not
-- mess with annotations
usingIntIO :: IntEnv -> IntIO a -> PropertyT IO a
usingIntIO env rdr = do
  -- NOTE: Evidently annotations do not work with uncaught exceptions.
  -- Therefore we catch any exceptions here, print out any relevant data,
  -- then use Hedgehog's failure to die.
  result <-
    usingIntIONoCatch env rdr `catchAny` \ex -> do
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

tests :: IO FilePath -> TestTree
tests testDir =
  testGroup
    "SafeRm"
    (testsBackend testDir <$> [minBound .. maxBound])

testsBackend :: IO FilePath -> Backend -> TestTree
testsBackend testDir b =
  testGroup
    (Backend.backendTestDesc b)
    [ delete b testDir,
      deleteSome b testDir,
      permDelete b testDir,
      permDeleteSome b testDir,
      restore b testDir,
      restoreSome b testDir,
      emptyTrash b testDir,
      metadata b testDir
    ]

delete :: Backend -> IO FilePath -> TestTree
delete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "All paths are deleted" "delete" $ do
    property $ do
      testDir <- getTestPath mtestDir "delete" backend
      α <- forAll (genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> ".trash"
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map αTest
      assertPathsExist αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      αTest ^. #set === indexOrigPaths

deleteSome :: Backend -> IO FilePath -> TestTree
deleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir "deleteSome" backend
      (α, β) <- forAll (gen2FileNameSets b)
      let toTestDir = USeq.map (testDir </>)

          αTest = toTestDir α
          trashDir = testDir </> ".trash"
          αTrash = mkTrashPaths backend trashDir α
          βTrash = mkTrashPaths backend trashDir β
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map αTest
      assertPathsExist αTest

      -- delete files
      -- should succeed on α and fail on β
      let toDelete = αTest `USeq.union` toTestDir β

      caughtEx <-
        tryCS @_ @FileNotFoundE $
          usingIntIONoCatch env (SafeRm.delete (USeq.map MkPathI toDelete))

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- should have printed exactly 1 message for each bad filename
      termLogs <- liftIO $ readIORef (env ^. #termLogsRef)
      length termLogs === length β

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist βTrash

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- index should exactly match α
      αTest ^. #set === indexOrigPaths

permDelete :: Backend -> IO FilePath -> TestTree
permDelete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDelete" $ do
    property $ do
      testDir <- getTestPath mtestDir "permDelete" backend
      α <- forAll (genFileNameSet b)
      let trashDir = testDir </> ".trash"
          αTest = USeq.map (testDir </>) α
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map αTest
      assertPathsExist αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- permanently delete files
      let toPermDelete = USeq.map MkPathI α
      usingIntIO env $ SafeRm.permDelete True toPermDelete

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      [] === index
      assertPathsDoNotExist (αTest `USeq.union` αTrash)
  where
    desc = "All trash entries are permanently deleted"

permDeleteSome :: Backend -> IO FilePath -> TestTree
permDeleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDeleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir "permDeleteSome" backend
      (α, β, γ) <- forAll (gen3FileNameSets b)
      let toTestDir = USeq.map (testDir </>)

          toDelete = toTestDir (α `USeq.union` γ)
          trashDir = testDir </> ".trash"
          trashSet = mkTrashPaths backend trashDir (α `USeq.union` γ)
          αTrash = mkTrashPaths backend trashDir α
          βTrash = mkTrashPaths backend trashDir β
          γTrash = mkTrashPaths backend trashDir γ
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map toDelete
      assertPathsExist toDelete

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist trashSet
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- permanently delete files
      -- should succeed on α and fail on β
      let toPermDelete = USeq.map MkPathI (α `USeq.union` β)
      annotateShow toPermDelete

      caughtEx <-
        tryCS @_ @TrashEntryNotFoundE $
          usingIntIONoCatch env (SafeRm.permDelete True toPermDelete)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      toTestDir γ ^. #set === indexOrigPaths

      annotate "Assert files do not exist"
      assertPathsDoNotExist (αTrash `USeq.union` βTrash)
      annotate "Assert files exist"
      assertPathsExist γTrash
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: Backend -> IO FilePath -> TestTree
restore backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Restores all trash entries" "restore" $ do
    property $ do
      testDir <- getTestPath mtestDir "restore" backend
      α <- forAll (genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> ".trash"
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map αTest
      assertPathsExist αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- restore files
      let toRestore = USeq.map MkPathI α
      usingIntIO env $ SafeRm.restore toRestore

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      [] === index
      annotate "Assert files exist"
      assertPathsExist αTest
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTrash

restoreSome :: Backend -> IO FilePath -> TestTree
restoreSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "restoreSome" $ do
    property $ do
      testDir <- getTestPath mtestDir "restoreSome" backend
      (α, β, γ) <- forAll (gen3FileNameSets b)
      let toTestDir = USeq.map (testDir </>)

          toDelete = toTestDir (α `USeq.union` γ)
          trashDir = testDir </> ".trash"
          αTrash = mkTrashPaths backend trashDir α
          γTrash = mkTrashPaths backend trashDir γ
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map toDelete
      assertPathsExist toDelete

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- restore
      -- should succeed on α and fail on β
      let toRestore = USeq.map MkPathI (α `USeq.union` β)
      annotateShow toRestore

      caughtEx <-
        tryCS @_ @TrashEntryNotFoundE $
          usingIntIONoCatch env (SafeRm.restore toRestore)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      toTestDir γ ^. #set === indexOrigPaths

      annotate "Assert files exist"
      assertPathsExist (toTestDir α `USeq.union` γTrash)
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTrash
  where
    desc = "Some trash entries are restored, others error"

emptyTrash :: Backend -> IO FilePath -> TestTree
emptyTrash backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Empties the trash" "empty" $ do
    property $ do
      testDir <- getTestPath mtestDir "emptyTrash" backend
      α <- forAll (genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> ".trash"
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map αTest
      assertPathsExist αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- empty trash
      usingIntIO env $ SafeRm.emptyTrash True

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      [] === index
      annotate "Assert files do not exist"
      assertPathsDoNotExist (αTest `USeq.union` αTrash)

metadata :: Backend -> IO FilePath -> TestTree
metadata backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Retrieves metadata" "metadata" $ do
    property $ do
      testDir <- getTestPath mtestDir "metadata" backend
      α <- forAll (genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> ".trash"
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map αTest
      assertPathsExist αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- get metadata
      metadata' <- usingIntIO env SafeRm.getMetadata

      length α === natToInt (metadata' ^. #numEntries)
      length α === natToInt (metadata' ^. #numFiles)

natToInt :: (HasCallStack) => Natural -> Int
natToInt i
  | i <= intMax = fromIntegral i
  | otherwise = error $ "natToInt: Too large to convert to Int: " <> show i
  where
    intMax = fromIntegral (maxBound :: Int)

genFileNameSet :: Bool -> Gen (UniqueSeq FilePath)
genFileNameSet asciiOnly = fromFoldable <$> Gen.list range (genFileName asciiOnly)
  where
    range = Range.linear 1 100

gen2FileNameSets :: Bool -> Gen (UniqueSeq FilePath, UniqueSeq FilePath)
gen2FileNameSets asciiOnly = do
  α <- fromFoldable <$> Gen.list range (genFileName asciiOnly)
  β <- fromFoldable <$> Gen.list range (genFileNameNoDupes asciiOnly α)
  pure (α, β)
  where
    range = Range.linear 1 100

gen3FileNameSets :: Bool -> Gen (UniqueSeq FilePath, UniqueSeq FilePath, UniqueSeq FilePath)
gen3FileNameSets asciiOnly = do
  α <- fromFoldable <$> Gen.list range (genFileName asciiOnly)
  β <- fromFoldable <$> Gen.list range (genFileNameNoDupes asciiOnly α)
  γ <- fromFoldable <$> Gen.list range (genFileNameNoDupes asciiOnly (α `USeq.union` β))
  pure (α, β, γ)
  where
    range = Range.linear 1 100

genFileName :: Bool -> Gen FilePath
genFileName asciiOnly = genFileNameNoDupes asciiOnly USeq.empty

genFileNameNoDupes :: Bool -> UniqueSeq FilePath -> Gen FilePath
genFileNameNoDupes asciiOnly paths =
  Gen.filter
    (not . (`USeq.member` paths))
    (Gen.string range (genChar asciiOnly))
  where
    range = Range.linear 1 20

genChar :: Bool -> Gen Char
genChar asciiOnly = Gen.filterT f (mapper <$> g)
  where
    g =
      if asciiOnly
        then Gen.ascii
        else Gen.unicode
#if OSX
    -- Below unrestricted unicode paths are causing the mac tests to fail.
    -- It would be nice to have a list of paths that we _should_ support,
    -- so we know which set to generate, but for now, just generate the
    -- printable chars.
    --
    -- Note: paths that previously caused failures were \x19ad and \x2800.
    -- ':' was also excluded, though it hadn't caused a test failure (yet)
    f c = Ch.isPrint c && notBadChar c

    -- The wildcard '*' is excluded because it causes tests to fail.
    -- For instance, suppose we delete files '*' and ' ' and then try to
    -- permanently delete them. Permanently deleting the wildcard will match
    -- ' ' and delete both, so then when we try to explicitly delete ' ',
    -- it will fail.
    notBadChar :: Char -> Bool
    notBadChar c = not $ L.elem @[] c ['/', '.', ':', '*']

    mapper :: Char -> Char
    mapper = id
#elif WINDOWS
    f c = Ch.isPrint c && notBadChar c

    -- https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#file-and-directory-names
    notBadChar :: Char -> Bool
    notBadChar c = not $ L.elem @[] c
      ['/', '\\', '<', '>', ':', '"', '|', '?', '*', '0', '.', ' ']

    -- windows paths are case-insensitive by default, so let's just take
    -- lower-case paths :-(
    mapper :: Char -> Char
    mapper = Ch.toLower
#else
    f c = notControl c && notBadChar c

    notControl :: Char -> Bool
    notControl = not . Ch.isControl

    notBadChar :: Char -> Bool
    notBadChar c = not $ L.elem @[] c ['/', '.', '*']

    mapper :: Char -> Char
    mapper = id
#endif

toOrigPath :: HashSet FilePath -> PathData -> HashSet FilePath
toOrigPath acc pd = HSet.insert (pd ^. #originalPath % #unPathI) acc

mkEnv :: Backend -> FilePath -> IO IntEnv
mkEnv backend fp = do
  termLogsRef <- newIORef []
  logsRef <- newIORef []

  pure $
    MkIntEnv
      { coreEnv =
          MkEnv
            { trashHome = MkPathI fp,
              backend,
              logEnv = MkLogEnv Nothing ""
            },
        termLogsRef,
        logsRef,
        namespace = "integration"
      }

mkTrashPaths ::
  ( Foldable f,
    IsList (f FilePath),
    Item (f FilePath) ~ FilePath
  ) =>
  Backend ->
  FilePath ->
  f FilePath ->
  f FilePath
mkTrashPaths backend trashHome =
  fromList . foldr (\p acc -> mkTrashPath p : mkTrashInfoPath p : acc) []
  where
    mkTrashPath p = trashHome </> "files" </> p
    mkTrashInfoPath p = trashHome </> "info" </> p <> Env.trashInfoExtension backend

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
getTestPath :: (MonadIO m) => IO Path -> Path -> Backend -> m Path
getTestPath mtestPath p backend = do
  -- liftIO (canonicalizePath =<< mtestPath)
  testPath <- liftIO $ canonicalizePath =<< mtestPath
  pure $ testPath </> p </> Backend.backendArg backend
