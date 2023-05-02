{-# LANGUAGE OverloadedLists #-}

-- | Runner unit tests.
module Unit.Runner
  ( tests,
  )
where

import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Index (Sort (Name))
import SafeRm.Data.PathData.Formatting (ColFormat (..), PathDataFormat (..))
import SafeRm.Runner (getConfiguration)
import SafeRm.Runner.Command
  ( _Delete,
    _Empty,
    _List,
    _Metadata,
    _PermDelete,
    _Restore,
  )
import SafeRm.Runner.Command.List (ListCmd (..))
import SafeRm.Runner.FileSizeMode (FileSizeMode (..))
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
      permDeleteForce,
      emptyTrash,
      emptyTrashForce,
      restore,
      list,
      listNonDefaults,
      listNonDefaultsNoFormat,
      metadata
    ]

delete :: TestTree
delete = testCase "Parses delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just ["foo", "bar"] @=? cmd ^? _Delete
  where
    argList = ["delete", "foo", "bar", "-c", "none"]

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just (False, ["foo", "bar"]) @=? cmd ^? _PermDelete
  where
    argList = ["perm-delete", "foo", "bar", "-c", "none"]

permDeleteForce :: TestTree
permDeleteForce = testCase "Parses perm delete with force" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just (True, ["foo", "bar"]) @=? cmd ^? _PermDelete
  where
    argList = ["perm-delete", "-f", "foo", "bar", "-c", "none"]

emptyTrash :: TestTree
emptyTrash = testCase "Parses empty" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just False @=? cmd ^? _Empty
  where
    argList = ["empty", "-c", "none"]

emptyTrashForce :: TestTree
emptyTrashForce = testCase "Parses empty with force" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just True @=? cmd ^? _Empty
  where
    argList = ["empty", "-f", "-c", "none"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just ["foo", "bar"] @=? cmd ^? _Restore
  where
    argList = ["restore", "foo", "bar", "-c", "none"]

list :: TestTree
list = testCase "Parses list" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
  where
    argList = ["list", "-c", "none"]
    defList =
      MkListCmd
        { format = FormatTabular Nothing Nothing,
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
        "100"
      ]
    defList =
      MkListCmd
        { format = FormatTabular (Just $ ColFormatFixed 80) (Just $ ColFormatFixed 100),
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
        { format = FormatTabular (Just $ ColFormatFixed 80) (Just ColFormatMax),
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

  Just "./tmp" @=? cfg ^. #trashHome
  Just BackendFdo @=? cfg ^. #backend
  Just (Just LevelInfo) @=? cfg ^. #logLevel
  Just (FileSizeModeWarn (MkBytes 10_000_000)) @=? cfg ^. #logSizeMode
  where
    argList = ["-c", "examples/config.toml", "delete", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "not-tmp" @=? cfg ^. #trashHome
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
