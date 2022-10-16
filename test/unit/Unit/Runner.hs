{-# LANGUAGE OverloadedLists #-}

-- | Runner unit tests.
--
-- @since 0.1
module Unit.Runner
  ( tests,
  )
where

import SafeRm.Runner (getConfiguration)
import SafeRm.Runner.Command
  ( _Delete,
    _DeletePerm,
    _Empty,
    _List,
    _Metadata,
    _Restore,
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
  Just () @=? cmd ^? _List
  where
    argList = ["l", "-c", "none"]

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
      argsOverridesToml,
      defaultConfig
    ]

parsesExample :: TestTree
parsesExample = testCase "Parses Example" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "./tmp" @=? cfg ^. #trashHome
  Just (Just LevelInfo) @=? cfg ^. #logLevel
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

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

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Nothing @=? cfg ^. #logLevel
  where
    argList =
      [ "d",
        "foo",
        "-c",
        "none"
      ]
