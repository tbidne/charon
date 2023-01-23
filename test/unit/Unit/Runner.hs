{-# LANGUAGE OverloadedLists #-}

-- | Runner unit tests.
--
-- @since 0.1
module Unit.Runner
  ( tests,
  )
where

import SafeRm.Data.Index (Sort (Name, Size))
import SafeRm.Data.PathData (PathDataFormat (FormatMultiline, FormatTabular, FormatTabularAuto))
import SafeRm.Runner (getConfiguration)
import SafeRm.Runner.Command
  ( ListCommand (MkListCommand, format, revSort, sort),
    _Delete,
    _DeletePerm,
    _Empty,
    _List,
    _Metadata,
    _Restore,
  )
import SafeRm.Runner.Config
  ( CmdListCfg
      ( MkCmdListCfg,
        format,
        nameTrunc,
        origTrunc,
        revSort,
        sort
      ),
    ListFormatCfg (FormatMultilineCfg),
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
      permDeleteForce,
      emptyTrash,
      emptyTrashForce,
      restore,
      list,
      listNonDefaults,
      metadata
    ]

delete :: TestTree
delete = testCase "Parses delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just ["foo", "bar"] @=? cmd ^? _Delete
  where
    argList = ["d", "foo", "bar", "-c", "none"]

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just (False, ["foo", "bar"]) @=? cmd ^? _DeletePerm
  where
    argList = ["x", "foo", "bar", "-c", "none"]

permDeleteForce :: TestTree
permDeleteForce = testCase "Parses perm delete with force" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just (True, ["foo", "bar"]) @=? cmd ^? _DeletePerm
  where
    argList = ["x", "-f", "foo", "bar", "-c", "none"]

emptyTrash :: TestTree
emptyTrash = testCase "Parses empty" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just False @=? cmd ^? _Empty
  where
    argList = ["e", "-c", "none"]

emptyTrashForce :: TestTree
emptyTrashForce = testCase "Parses empty with force" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just True @=? cmd ^? _Empty
  where
    argList = ["e", "-f", "-c", "none"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just ["foo", "bar"] @=? cmd ^? _Restore
  where
    argList = ["r", "foo", "bar", "-c", "none"]

list :: TestTree
list = testCase "Parses list" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just defList @=? cmd ^? _List
  where
    argList = ["l", "-c", "none"]
    defList =
      MkListCommand
        { format = FormatTabularAuto,
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
        "l",
        "--format",
        "t",
        "--name-trunc",
        "80",
        "--orig-trunc",
        "100"
      ]
    defList =
      MkListCommand
        { format = FormatTabular 80 100,
          sort = Name,
          revSort = False
        }

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just () @=? cmd ^? _Metadata
  where
    argList = ["m", "-c", "none"]

tomlTests :: TestTree
tomlTests =
  testGroup
    "Toml"
    [ parsesExample,
      usesListCfg,
      usesListCfgTabular,
      argsOverridesToml,
      argsOverridesTomlList,
      defaultConfig
    ]

parsesExample :: TestTree
parsesExample = testCase "Parses Example" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "./tmp" @=? cfg ^. #trashHome
  Just (Just LevelInfo) @=? cfg ^. #logLevel
  Just listCfg @=? cfg ^. #listCommand
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]
    listCfg =
      MkCmdListCfg
        { format = Just FormatMultilineCfg,
          nameTrunc = Just 80,
          origTrunc = Just 100,
          sort = Just Size,
          revSort = Just True
        }

usesListCfg :: TestTree
usesListCfg = testCase "Toml config copied into list cmd" $ do
  (_, cmd) <- SysEnv.withArgs argList getConfiguration

  Just FormatMultiline @=? cmd ^? (_List % #format)
  Just Size @=? cmd ^? (_List % #sort)
  Just True @=? cmd ^? (_List % #revSort)
  where
    argList = ["-c", "examples/config.toml", "l"]

usesListCfgTabular :: TestTree
usesListCfgTabular = testCase "Toml tabular config copied into list cmd" $ do
  (_, cmd) <- SysEnv.withArgs argList getConfiguration

  Just (FormatTabular 25 75) @=? cmd ^? (_List % #format)
  where
    argList = ["-c", "test/unit/config.toml", "l"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "not-tmp" @=? cfg ^. #trashHome
  Just (Just LevelError) @=? cfg ^. #logLevel
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--log-level",
        "error",
        "d",
        "foo"
      ]

argsOverridesTomlList :: TestTree
argsOverridesTomlList = testCase "Args overrides Toml list cmd" $ do
  (_, cmd) <- SysEnv.withArgs argList getConfiguration

  Just (FormatTabular 30 40) @=? cmd ^? (_List % #format)
  Just Name @=? cmd ^? (_List % #sort)
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "l",
        "--format",
        "tabular",
        "--name-trunc",
        "30",
        "--orig-trunc",
        "40",
        "--sort",
        "name"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Nothing @=? cfg ^. #logLevel
  Nothing @=? cfg ^. #listCommand
  where
    argList =
      [ "-c",
        "none",
        "d",
        "foo"
      ]
