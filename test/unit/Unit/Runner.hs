{-# LANGUAGE QuasiQuotes #-}

-- | Runner unit tests.
module Unit.Runner
  ( tests,
  )
where

import Charon.Backend.Data (Backend (BackendCbor, BackendFdo))
import Charon.Data.Index (Sort (Name))
import Charon.Data.PathData.Formatting
  ( ColFormat (ColFormatFixed, ColFormatMax),
    Coloring (ColoringDetect, ColoringOff, ColoringOn),
    PathDataFormat (FormatMultiline, FormatSingleline, FormatTabular),
  )
import Charon.Data.Paths (PathI (MkPathI))
import Charon.Data.UniqueSeqNE ((↤))
import Charon.Data.UniqueSeqNE qualified as UniqueSeqNE
import Charon.Runner (getConfiguration)
import Charon.Runner.Command
  ( IndicesPathsStrategy (PathsStrategy),
    NoPrompt (MkNoPrompt),
    _Delete,
    _Empty,
    _List,
    _Metadata,
    _PathsStrategy,
    _PermDelete,
    _Restore,
  )
import Charon.Runner.Command.List
  ( ListCmd
      ( MkListCmd,
        format,
        revSort,
        sort
      ),
  )
import Charon.Runner.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeWarn
      ),
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
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just expectedUSeq @=? cmd ^? _Delete
  where
    argList = ["delete", "foo", "bar", "-c", "none"]
    expectedUSeq =
      MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just (MkNoPrompt False, expectedUSeq) @=? cmd ^? _PermDelete
  where
    argList = ["perm-delete", "foo", "bar", "-c", "none"]
    expectedUSeq =
      PathsStrategy
        $ MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

permDeleteNoPrompt :: TestTree
permDeleteNoPrompt = testCase "Parses perm delete with --no-prompt" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just (MkNoPrompt True, expectedUSeq) @=? cmd ^? _PermDelete
  where
    argList = ["perm-delete", "--no-prompt", "foo", "bar", "-c", "none"]
    expectedUSeq =
      PathsStrategy
        $ MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

emptyTrash :: TestTree
emptyTrash = testCase "Parses empty" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just False @=? cmd ^? _Empty % #unNoPrompt
  where
    argList = ["empty", "-c", "none"]

emptyTrashNoPrompt :: TestTree
emptyTrashNoPrompt = testCase "Parses empty with --no-prompt" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just True @=? cmd ^? _Empty % #unNoPrompt
  where
    argList = ["empty", "--no-prompt", "-c", "none"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just expectedUSeq @=? cmd ^? (_Restore % #strategy % _PathsStrategy)
  where
    argList = ["restore", "foo", "bar", "-c", "none"]
    expectedUSeq =
      MkPathI
        ↤ UniqueSeqNE.fromNonEmpty ([osp|foo|] :| [[osp|bar|]])

list :: TestTree
list = testCase "Parses list" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
  where
    argList = ["list", "-c", "none"]
    defList =
      MkListCmd
        { format = FormatTabular ColoringDetect Nothing Nothing,
          sort = Name,
          revSort = False
        }

listNonDefaults :: TestTree
listNonDefaults = testCase "List non-default args" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
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
      MkListCmd
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
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
  where
    argList =
      [ "-c",
        "none",
        "list",
        "--format",
        "m"
      ]
    defList =
      MkListCmd
        { format = FormatMultiline,
          sort = Name,
          revSort = False
        }

listSingleline :: TestTree
listSingleline = testCase "List singleline" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
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
      MkListCmd
        { format = FormatSingleline ColoringOff,
          sort = Name,
          revSort = False
        }

listNonDefaultsNoFormat :: TestTree
listNonDefaultsNoFormat = testCase "List overrides args w/o format specified" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
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
      MkListCmd
        { format = FormatTabular ColoringDetect (Just $ ColFormatFixed 80) (Just ColFormatMax),
          sort = Name,
          revSort = False
        }

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just () @=? cmd ^? _Metadata
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
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just (MkPathI [osp|./tmp|]) @=? cfg ^. #trashHome
  Just BackendFdo @=? cfg ^. #backend
  Just (Just LevelInfo) @=? cfg ^. #logLevel
  Just (FileSizeModeWarn (MkBytes 10_000_000)) @=? cfg ^. #logSizeMode
  where
    argList = ["-c", "examples/config.toml", "delete", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just (MkPathI [osp|not-tmp|]) @=? cfg ^. #trashHome
  Just BackendCbor @=? cfg ^. #backend
  Just (Just LevelError) @=? cfg ^. #logLevel
  Just (FileSizeModeDelete (MkBytes 5_000_000)) @=? cfg ^. #logSizeMode
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
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just Nothing @=? cfg ^. #logLevel
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
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Nothing @=? cfg ^. #logLevel
  where
    argList =
      [ "-c",
        "none",
        "delete",
        "foo"
      ]
