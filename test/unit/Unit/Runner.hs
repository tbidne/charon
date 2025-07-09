{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Runner unit tests.
module Unit.Runner
  ( tests,
  )
where

import Charon.Backend.Data (Backend (BackendCbor, BackendFdo))
import Charon.Data.Index (Sort (Name, OriginalPath))
import Charon.Data.PathData.Formatting
  ( ColFormat (ColFormatFixed, ColFormatMax),
    Coloring (ColoringDetect, ColoringOff, ColoringOn),
    PathDataFormat (FormatMultiline, FormatSingleline, FormatTabular),
  )
import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import Charon.Data.UniqueSeqNE ((↤))
import Charon.Data.UniqueSeqNE qualified as UniqueSeqNE
import Charon.Runner (getConfiguration)
import Charon.Runner.Command
  ( _Delete,
    _Empty,
    _List,
    _Metadata,
    _PermDelete,
    _Restore,
  )
import Charon.Runner.Command.List
  ( ListParams
      ( MkListParams,
        format,
        revSort,
        sort
      ),
  )
import Charon.Runner.Config (_LogLevelOn)
import Charon.Runner.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeWarn
      ),
  )
import Charon.Runner.Merged
import Charon.Runner.Phase (Prompt (MkPrompt), _PathsStrategy)
import Effects.FileSystem.PathReader
  ( MonadPathReader (getXdgDirectory),
    XdgDirectory (XdgData),
  )
import System.Environment qualified as SysEnv
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Runner"
    [ argsTests,
      tomlTests
    ]

argsTests :: TestTree
argsTests =
  testGroup
    "Args"
    [ delete,
      permDelete,
      permDeleteNoPrompt,
      emptyTrash,
      emptyTrashNoPrompt,
      restore,
      list,
      listNonDefaults,
      listNonDefaultsNoFormat,
      listMultiline,
      listSingleline,
      metadata
    ]

delete :: TestTree
delete = testCase "Parses delete" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just expectedUSeq @=? cfg ^? (#command % _Delete % #paths)
  where
    argList = ["delete", "foo", "bar", "-c", "none"]
    expectedUSeq =
      MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just (MkPrompt True) @=? cfg ^? (#command % _PermDelete % #prompt)
  Just expectedUSeq @=? cfg ^? (#command % _PermDelete % #strategy % _PathsStrategy)
  where
    argList = ["perm-delete", "foo", "bar", "-c", "none"]
    expectedUSeq =
      MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

permDeleteNoPrompt :: TestTree
permDeleteNoPrompt = testCase "Parses perm delete with --no-prompt" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just (MkPrompt False) @=? cfg ^? (#command % _PermDelete % #prompt)
  Just expectedUSeq @=? cfg ^? (#command % _PermDelete % #strategy % _PathsStrategy)
  where
    argList = ["perm-delete", "--no-prompt", "foo", "bar", "-c", "none"]
    expectedUSeq =
      MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

emptyTrash :: TestTree
emptyTrash = testCase "Parses empty" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just True @=? cfg ^? #command % _Empty % #unPrompt
  where
    argList = ["empty", "-c", "none"]

emptyTrashNoPrompt :: TestTree
emptyTrashNoPrompt = testCase "Parses empty with --no-prompt" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just False @=? cfg ^? #command % _Empty % #unPrompt
  where
    argList = ["empty", "--no-prompt", "-c", "none"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just expectedUSeq @=? cfg ^? (#command % _Restore % #strategy % _PathsStrategy)
  where
    argList = ["restore", "foo", "bar", "-c", "none"]
    expectedUSeq =
      MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

list :: TestTree
list = testCase "Parses list" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just defList @=? cfg ^? #command % _List
  where
    argList = ["list", "-c", "none"]
    defList =
      MkListParams
        { format = FormatTabular ColoringDetect Nothing Nothing,
          sort = Name,
          revSort = False
        }

listNonDefaults :: TestTree
listNonDefaults = testCase "List non-default args" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just defList @=? cfg ^? #command % _List
  where
    argList =
      [ "-c",
        "none",
        "list",
        "--format",
        "t",
        "--name-len",
        "80",
        "--orig-len",
        "100",
        "--color",
        "true"
      ]
    defList =
      MkListParams
        { format =
            FormatTabular
              ColoringOn
              (Just $ ColFormatFixed 80)
              (Just $ ColFormatFixed 100),
          sort = Name,
          revSort = False
        }

listMultiline :: TestTree
listMultiline = testCase "List multiline" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just defList @=? cfg ^? #command % _List
  where
    argList =
      [ "-c",
        "none",
        "list",
        "--format",
        "m"
      ]
    defList =
      MkListParams
        { format = FormatMultiline,
          sort = Name,
          revSort = False
        }

listSingleline :: TestTree
listSingleline = testCase "List singleline" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just defList @=? cfg ^? #command % _List
  where
    argList =
      [ "-c",
        "none",
        "list",
        "--format",
        "s",
        "--color",
        "false"
      ]
    defList =
      MkListParams
        { format = FormatSingleline ColoringOff,
          sort = OriginalPath,
          revSort = False
        }

listNonDefaultsNoFormat :: TestTree
listNonDefaultsNoFormat = testCase "List overrides args w/o format specified" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just defList @=? cfg ^? #command % _List
  where
    argList =
      [ "-c",
        "none",
        "list",
        "--name-len",
        "80",
        "--orig-len",
        "max"
      ]
    defList =
      MkListParams
        { format = FormatTabular ColoringDetect (Just $ ColFormatFixed 80) (Just ColFormatMax),
          sort = Name,
          revSort = False
        }

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Just () @=? cfg ^? #command % _Metadata
  where
    argList = ["metadata", "-c", "none"]

tomlTests :: TestTree
tomlTests =
  testGroup
    "Toml"
    [ parsesExample,
      argsOverridesToml,
      argsDisablesTomlLogging,
      defaultConfig
    ]

parsesExample :: TestTree
parsesExample = testCase "Parses example" $ do
  cfg <- runConfig argList

  MkPathI [osp|./tmp|] @=? cfg ^. #coreConfig % #trashHome
  BackendFdo @=? cfg ^. #coreConfig % #backend
  Just LevelInfo @=? cfg ^? #coreConfig % #logging %? #logLevel % _LogLevelOn
  Just (FileSizeModeWarn (MkBytes 10_000_000)) @=? cfg ^? #coreConfig % #logging %? #logSizeMode
  where
    argList = ["-c", "examples/config.toml", "delete", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  cfg <- runConfig argList

  MkPathI [osp|not-tmp|] @=? cfg ^. #coreConfig % #trashHome
  BackendCbor @=? cfg ^. #coreConfig % #backend
  Just LevelError @=? cfg ^? #coreConfig % #logging %? #logLevel % _LogLevelOn
  Just (FileSizeModeDelete (MkBytes 5_000_000)) @=? cfg ^? #coreConfig % #logging %? #logSizeMode
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "-b",
        "cbor",
        "--log-level",
        "error",
        "--log-size-mode",
        "delete 5 mb",
        "delete",
        "foo"
      ]

argsDisablesTomlLogging :: TestTree
argsDisablesTomlLogging = testCase "Args disables Toml logging" $ do
  cfg <- runConfig argList

  Nothing @=? cfg ^. #coreConfig % #logging
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "--log-level",
        "none",
        "delete",
        "foo"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  cfg <- runConfig argList

  defTrashHome @=? cfg ^. (#coreConfig % #trashHome)
  Nothing @=? cfg ^. (#coreConfig % #logging)
  where
    argList =
      [ "-c",
        "none",
        "delete",
        "foo"
      ]

newtype MockIO a = MkMockIO (IO a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadFileReader,
      MonadOptparse,
      MonadThrow
    )

instance MonadPathReader MockIO where
  getXdgDirectory XdgData p = pure $ [osp|xdg_data|] </> p
  getXdgDirectory other _ = error $ "unexpected xdg: " ++ show other

instance MonadTerminal MockIO

runMockIO :: MockIO a -> IO a
runMockIO (MkMockIO io) = io

runConfig :: [String] -> IO MergedConfig
runConfig argList = do
  SysEnv.withArgs argList (runMockIO getConfiguration)

defTrashHome :: PathI TrashHome
defTrashHome = MkPathI [ospPathSep|xdg_data/charon|]
