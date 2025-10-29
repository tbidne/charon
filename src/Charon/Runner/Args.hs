{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides CLI args functionality.
module Charon.Runner.Args
  ( getArgs,
    Args (..),
    TomlConfigPath (..),
  )
where

import Charon.Backend.Data (Backend, parseBackend)
import Charon.Data.Index (Sort, readSort)
import Charon.Data.PathData.Formatting
  ( ColFormat (ColFormatFixed, ColFormatMax),
    Coloring (ColoringDetect, ColoringOff, ColoringOn),
  )
import Charon.Data.PathData.Formatting qualified as PathData
import Charon.Data.Paths (PathIndex (TrashHome), RawPathI (MkRawPathI))
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Data.UniqueSeqNE qualified as UniqueSeqNE
import Charon.Prelude
import Charon.Runner.Args.TH qualified as TH
import Charon.Runner.Command
  ( Command
      ( Convert,
        Delete,
        Empty,
        List,
        Merge,
        Metadata,
        PermDelete,
        Restore
      ),
  )
import Charon.Runner.Command.Delete
  ( DeleteParams
      ( MkDeleteParams,
        paths,
        prompt,
        verbose
      ),
  )
import Charon.Runner.Command.List
  ( ListFormatPhase1 (MkListFormatPhase1),
    ListFormatStyle,
    ListParams (MkListParams),
    parseListFormat,
  )
import Charon.Runner.Command.PermDelete
  ( PermDeleteParams
      ( MkPermDeleteParams,
        prompt,
        strategy,
        verbose
      ),
  )
import Charon.Runner.Command.Restore
  ( RestoreParams
      ( MkRestoreParams,
        force,
        prompt,
        strategy,
        verbose
      ),
  )
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LogLevelConfig,
    LoggingConfig (MkLoggingConfig, logLevel, logSizeMode),
  )
import Charon.Runner.Config qualified as Config
import Charon.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseArgs),
    Force (MkForce),
    Prompt (MkPrompt),
    Verbose (MkVerbose),
  )
import Charon.Runner.Phase qualified as Phase
import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version (showVersion)
import Effects.Optparse (osPath)
import Effects.Optparse.Completer qualified as EOC
import FileSystem.OsString qualified as OsString
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    OptionFields,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Doc)
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_charon qualified as Paths
import System.Info qualified as Info
import Text.Read qualified as TR

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

makeFieldLabelsNoPrefix ''VersionInfo

-- | Toml path config.
data TomlConfigPath
  = -- | Do not use any Toml config.
    TomlNone
  | -- | Attempts to read the Toml file at the default path.
    TomlDefault
  | -- | Path to Toml file.
    TomlPath OsPath
  deriving stock
    ( Eq,
      Show
    )

-- | CLI args.
data Args = MkArgs
  { -- | Command to run.
    command :: Command ConfigPhaseArgs,
    -- | Core config.
    coreConfig :: CoreConfig ConfigPhaseArgs,
    -- | Path to toml config.
    tomlConfigPath :: TomlConfigPath
  }
  deriving stock (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Args

-- | Retrieves CLI args.
getArgs :: (MonadOptparse m) => m Args
getArgs = execParser parserInfoArgs

parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Charon: A tool for deleting files to a trash directory."
    footerTxt = Just $ fromString versShort
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Charon moves files to a trash directory, so they can later be ",
            "restored or permanently deleted. It is intended as a safer ",
            "alternative to rm. See github.com/tbidne/charon#readme for ",
            "full documentation."
          ]

argsParser :: Parser Args
argsParser = p <**> version <**> OA.helper
  where
    p :: Parser Args
    p = do
      command <- commandParser

      backend <- backendParser
      logLevel <- logLevelParser
      logSizeMode <- logSizeModeParser
      trashHome <- trashParser

      tomlConfigPath <- configParser
      pure
        $ MkArgs
          { command,
            coreConfig =
              MkCoreConfig
                { backend,
                  logging =
                    MkLoggingConfig
                      { logLevel,
                        logSizeMode
                      },
                  trashHome
                },
            tomlConfigPath
          }

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.hidden)

backendParser :: Parser (Maybe Backend)
backendParser =
  OA.optional
    $ OA.option (OA.str >>= parseBackend)
    $ mconcat
      [ OA.long "backend",
        OA.short 'b',
        OA.metavar "(cbor | fdo | json)",
        OA.completeWith ["cbor", "fdo", "json"],
        helpTxt
      ]
  where
    helpTxt =
      itemize
        [ intro,
          cbor,
          fdo,
          js
        ]

    intro = "Backend to use with charon. This option affects how path metadata is stored. Options are: "
    cbor = "cbor: Space efficient, not inspectable."
    fdo = "fdo: Compatible with FreeDesktop.org."
    js = "json: Inspectable."

backendDestParser :: Parser Backend
backendDestParser =
  OA.option (OA.str >>= parseBackend)
    $ mconcat
      [ OA.long "dest",
        OA.short 'd',
        OA.metavar "(cbor | fdo | json)",
        OA.completeWith ["cbor", "fdo", "json"],
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Backend to which we convert the current backend. See --backend ",
          "for more details."
        ]

configParser :: Parser TomlConfigPath
configParser =
  OA.option
    readTomlPath
    $ mconcat
      [ OA.value TomlDefault,
        OA.long "config",
        OA.short 'c',
        OA.metavar "(PATH | off)",
        OA.completeWith ["off"],
        OA.completer EOC.compgenCwdPathsCompleter,
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the toml config file. Can be the string 'off' -- in which ",
          "case no toml config is used -- or a path to the config file. If ",
          "not specified then we look in the XDG config directory ",
          "e.g. ~/.config/charon/config.toml"
        ]
    readTomlPath = do
      p <- osPath
      if p == [osp|off|]
        then pure TomlNone
        else pure $ TomlPath p

commandParser :: Parser (Command ConfigPhaseArgs)
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "delete" delParser delTxt,
          mkCommand "perm-delete" permDelParser permDelTxt,
          mkCommand "empty" emptyParser emptyTxt,
          OA.commandGroup "Delete commands:"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "restore" restoreParser restoreTxt,
            OA.commandGroup "Restore commands:",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "list" listParser listTxt,
            mkCommand "metadata" metadataParser metadataTxt,
            OA.commandGroup "Information commands:",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "convert" convertParser convertTxt,
            mkCommand "merge" mergeParser mergeTxt,
            OA.commandGroup "Transform commands:",
            OA.hidden
          ]
      )
  where
    delTxt = mkCmdDescStr "Moves the path(s) to the trash."
    permDelTxt =
      mkCmdDesc
        $ Chunk.vsepChunks
          [ Chunk.paragraph
              $ mconcat
                [ "Permanently deletes path(s) from the trash. ",
                  "Can be run with explicit paths, wildcards, or --indices."
                ],
            Chunk.paragraph "Examples:",
            mkExample
              [ "Deleting explicit paths f1 f2 f3:",
                "$ charon perm-delete f1 f2 f3"
              ],
            mkExample
              [ "Wildcard search; matches foobar, xxxfooyyybar, etc:",
                "$ charon perm-delete '*foo*bar'"
              ],
            mkExample
              [ "Prints out trash index first, allows delete via numeric indices:",
                "$ charon perm-delete --indices"
              ]
          ]
    emptyTxt = mkCmdDescStrNoLine "Empties the trash."
    restoreTxt =
      mkCmdDescNoLine
        $ Chunk.vsepChunks
          [ Chunk.paragraph
              $ mconcat
                [ "Restores the trash path(s) to their original location. ",
                  "Can be run with explicit paths, wildcards, or --indices."
                ],
            Chunk.paragraph "Examples:",
            mkExample
              [ "Restoring explicit paths f1 f2 f3:",
                "$ charon restore f1 f2 f3"
              ],
            mkExample
              [ "Wildcard search; matches foobar, xxxfooyyybar, etc:",
                "$ charon restore '*foo*bar'"
              ],
            mkExample
              [ "Prints out trash index first, allows restore via numeric indices:",
                "$ charon restore --indices"
              ]
          ]
    listTxt = mkCmdDescStr "Lists all trash contents."
    metadataTxt = mkCmdDescStrNoLine "Prints trash metadata."
    convertTxt = mkCmdDescStr "Converts the backend."
    mergeTxt = mkCmdDescStrNoLine "Merges src (implicit or -t) trash home into dest. Collisions will throw an error."

    mkExample :: [String] -> Chunk Doc
    mkExample =
      Chunk.vcatChunks
        . fmap (fmap (Pretty.indent 2) . Chunk.paragraph)

    delParser =
      Delete <$> do
        paths <- pathsParser
        prompt <- promptParser "Prompts before deleting path(s). Defaults to 'off'."
        verbose <- verboseParser "Lists deleted paths."
        pure
          $ MkDeleteParams
            { paths,
              prompt,
              verbose
            }
    permDelParser =
      PermDelete <$> do
        indices <- indicesParser
        prompt <- promptParser "Prompts before deleting path(s). Defaults to 'on'."
        verbose <- verboseParser "Lists deleted paths."
        paths <- mPathsParser
        pure
          $ MkPermDeleteParams
            { prompt,
              strategy = (indices, paths),
              verbose
            }
    emptyParser =
      Empty
        <$> promptParser "Prompts before emptying the trash. Defaults to 'on'."
    restoreParser =
      Restore <$> do
        force <- forceParser restoreForceTxt
        indices <- indicesParser
        prompt <- promptParser restorePromptTxt
        verbose <- verboseParser "Lists restored paths."
        paths <- mPathsParser
        pure
          $ MkRestoreParams
            { force,
              prompt,
              strategy = (indices, paths),
              verbose
            }
    restoreForceTxt =
      mconcat
        [ "Forcibly overwrites restored path(s). Otherwise, ",
          "collisions with existing paths will either throw an error ",
          "(with --prompt off) or prompt the user to decide."
        ]
    restorePromptTxt = "Prompts before restoring path(s). Defaults to 'on'."
    listParser =
      fmap List
        $ MkListParams
        <$> ( MkListFormatPhase1
                <$> coloringParser
                <*> listFormatStyleParser
                <*> nameTruncParser
                <*> origTruncParser
            )
        <*> sortParser
        <*> reverseSortParser
    metadataParser = pure Metadata
    convertParser = Convert <$> backendDestParser
    mergeParser = Merge <$> trashDestParser

indicesParser :: Parser (Maybe Bool)
indicesParser = switchParserOpts opts id "indices" helpTxt
  where
    helpTxt =
      mconcat
        [ "Allows selecting by numeric index instead of trash name. ",
          "Incompatible with explicit paths. The prompt can be exited ",
          "via 'exit', 'quit', 'q', or ':q'."
        ]

    opts = OA.short 'i'

listFormatStyleParser :: Parser (Maybe ListFormatStyle)
listFormatStyleParser =
  A.optional
    $ OA.option (OA.str >>= parseListFormat)
    $ mconcat
      [ OA.long "format",
        OA.metavar "FMT",
        OA.completeWith ["multi", "single", "tabular", "tabular-simple"],
        helpTxt
      ]
  where
    helpTxt =
      itemize
        [ intro,
          multi,
          single,
          tabular,
          tabularSimple
        ]
    intro = "Formatting options."
    tabular =
      mconcat
        [ "tabular: The default. Prints a table that tries to ",
          "intelligently size the table based on available terminal ",
          "width and filename / original path lengths."
        ]
    tabularSimple =
      mconcat
        [ "tabular-simple: Simple table that does no resizing. Prints ",
          "the table with indices."
        ]
    multi = "multi: Prints each entry across multiple lines."
    single = "single: Compact, prints each entry across a single lines."

toMDoc :: String -> Maybe Doc
toMDoc = Chunk.unChunk . Chunk.paragraph

nameTruncParser :: Parser (Maybe ColFormat)
nameTruncParser = colParser PathData.formatFileNameLenMin fields
  where
    fields =
      mconcat
        [ OA.long "name-len",
          OA.short 'n',
          OA.metavar "(max | NAT)",
          OA.completeWith ["max"],
          mkHelp
            $ mconcat
              [ "Sets the file name column length to either NAT characters or ",
                "longest file-name. Only affects the 'tabular' format."
              ]
        ]

origTruncParser :: Parser (Maybe ColFormat)
origTruncParser = colParser PathData.formatOriginalPathLenMin fields
  where
    fields =
      mconcat
        [ OA.long "orig-len",
          OA.short 'o',
          OA.metavar "(max | NAT)",
          OA.completeWith ["max"],
          mkHelp
            $ mconcat
              [ "Sets the original-path column length to either NAT characters or ",
                "longest path. Only affects the 'tabular' format."
              ]
        ]

colParser :: Natural -> Mod OptionFields ColFormat -> Parser (Maybe ColFormat)
colParser minLen = A.optional . OA.option readCol
  where
    readCol =
      OA.str >>= \case
        "max" -> pure ColFormatMax
        other -> case TR.readMaybe other of
          Just n -> pure $ ColFormatFixed n
          Nothing ->
            fail
              $ mconcat
                [ "Unrecognized col-format. Should either be 'max' or a positive ",
                  "integer < ",
                  show minLen,
                  ". Received: ",
                  other
                ]

coloringParser :: Parser (Maybe Coloring)
coloringParser =
  A.optional
    $ OA.option readColoring
    $ mconcat
      [ OA.long "color",
        OA.completeWith ["detect", "on", "off"],
        helpTxt
      ]
  where
    readColoring =
      OA.str >>= \case
        "on" -> pure ColoringOn
        "off" -> pure ColoringOff
        "detect" -> pure ColoringDetect
        bad -> fail $ "Unexpected --coloring: " ++ bad

    helpTxt =
      itemize
        [ "Coloring options.",
          "on",
          "off",
          "detect: On if supported."
        ]

sortParser :: Parser (Maybe Sort)
sortParser =
  A.optional
    $ OA.option
      (OA.str >>= readSort)
    $ mconcat
      [ OA.long "sort",
        OA.short 's',
        OA.metavar "(name | size)",
        OA.completeWith ["name", "size"],
        mkHelp "How to sort the list. Defaults to name."
      ]

reverseSortParser :: Parser (Maybe Bool)
reverseSortParser =
  A.optional
    $ OA.flag' True
    $ mconcat
      [ OA.long "reverse-sort",
        OA.short 'r',
        mkHelp helpTxt
      ]
  where
    helpTxt = "Sorts in the reverse order."

forceParser :: String -> Parser (Maybe Force)
forceParser = switchParser MkForce "force"

promptParser :: String -> Parser (Maybe Prompt)
promptParser = switchParser MkPrompt "prompt"

verboseParser :: String -> Parser (Maybe Verbose)
verboseParser = switchParserOpts opts MkVerbose "verbose"
  where
    opts = OA.short 'v'

trashParser :: Parser (Maybe (RawPathI TrashHome))
trashParser =
  A.optional
    $ OA.option
      (fmap MkRawPathI osPath)
    $ mconcat
      [ OA.long "trash-home",
        OA.short 't',
        OA.metavar "PATH",
        OA.completer EOC.compgenCwdPathsCompleter,
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the trash directory. This overrides the toml config, if ",
          "it exists. If neither is given then we use the XDG data directory ",
          "e.g. ~/.local/share/charon."
        ]

trashDestParser :: Parser (RawPathI TrashHome)
trashDestParser =
  OA.option
    (fmap MkRawPathI osPath)
    $ mconcat
      [ OA.long "dest",
        OA.short 'd',
        OA.metavar "PATH",
        OA.completer EOC.compgenCwdPathsCompleter,
        mkHelp helpTxt
      ]
  where
    helpTxt = "Path to the dest trash directory."

logLevelParser :: Parser (Maybe LogLevelConfig)
logLevelParser =
  A.optional
    $ OA.option (OA.str >>= Config.readLogLevel)
    $ mconcat
      [ OA.long "log-level",
        OA.metavar Config.logLevelStrings,
        OA.completeWith ["debug", "info", "warn", "error", "fatal", "off"],
        mkHelp
          $ mconcat
            [ "The file level in which to log. Defaults to off. Logs are ",
              "written to the XDG state directory e.g. ~/.local/state/charon."
            ]
      ]

mPathsParser :: Parser (Maybe (UniqueSeqNE (RawPathI i)))
mPathsParser = A.optional pathsParser

pathsParser :: Parser (UniqueSeqNE (RawPathI i))
pathsParser =
  -- NOTE: _should_ be safe because OA.some only succeeds for non-zero input.
  -- We do this rather than using NonEmpty's some1 because otherwise the CLI
  -- help metavar is duplicated i.e. "PATHS... [PATHS...]".
  --
  -- Also we explicitly favor 'osPath' over 'validOsPath' i.e. we do NOT
  -- want path validation here. The reason is that we want to allow users to
  -- pass paths containing wildcards (*) for easier matching, but these are
  -- not valid windows paths, hence will fail any validation checks.
  UniqueSeqNE.fromNonEmpty
    . unsafeNE
    <$> OA.some (OA.argument (fmap MkRawPathI osPath) (OA.metavar "PATHS..."))

logSizeModeParser :: Parser (Maybe FileSizeMode)
logSizeModeParser =
  OA.optional
    $ OA.option
      readFileSize
      ( mconcat
          [ OA.long "log-size-mode",
            mkHelp helpTxt,
            OA.completeWith ["delete", "warn"],
            OA.metavar "(warn SIZE | delete SIZE)"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Sets a threshold for the file log size, upon which we either ",
          "print a warning or delete the file, if it is exceeded. ",
          "The SIZE should include the value and units e.g. ",
          "'warn 10 mb', 'warn 5 gigabytes', 'delete 20.5B'."
        ]
    readFileSize = OA.str >>= parseFileSizeMode

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

versShort :: String
versShort =
  mconcat
    [ "Version: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient $ versionInfo ^. #gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Charon: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient (versionInfo ^. #gitHash),
      " - Commit date:  " <> OsString.decodeLenient (versionInfo ^. #gitCommitDate),
      " - GHC version:  " <> versionInfo ^. #ghc
    ]

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.compilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

unsafeNE :: (HasCallStack) => [a] -> NonEmpty a
unsafeNE [] = error "Args: Empty list given to unsafeNE"
unsafeNE (x : xs) = x :| xs

-- Looks a bit convoluted, but this gets us what we want:
-- 1. lines aligned (paragraph)
-- 2. linebreak at the end (fmap hardline)
mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkHelpNoLine :: String -> OA.Mod f a
mkHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.paragraph

mkCmdDescStr :: String -> InfoMod a
mkCmdDescStr = mkCmdDesc . Chunk.paragraph

mkCmdDescStrNoLine :: String -> InfoMod a
mkCmdDescStrNoLine = mkCmdDescNoLine . Chunk.paragraph

mkCmdDesc :: Chunk Doc -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk

-- For the last command, so we do not append two lines (there is an automatic
-- one at the end).
mkCmdDescNoLine :: Chunk Doc -> InfoMod a
mkCmdDescNoLine =
  OA.progDescDoc
    . Chunk.unChunk

switchParser :: (Bool -> a) -> String -> String -> Parser (Maybe a)
switchParser = switchParserHelper mkHelp mempty

switchParserOpts :: Mod OA.OptionFields Bool -> (Bool -> a) -> String -> String -> Parser (Maybe a)
switchParserOpts = switchParserHelper mkHelp

switchParserNoLine :: (Bool -> a) -> String -> String -> Parser (Maybe a)
switchParserNoLine = switchParserHelper mkHelpNoLine mempty

switchParserHelper ::
  -- | Help function, determines final newlines behavior.
  (String -> Mod OA.OptionFields Bool) ->
  -- | Additional options.
  Mod OA.OptionFields Bool ->
  -- | Type constructor.
  (Bool -> a) ->
  -- | Option name.
  String ->
  -- | Help text.
  String ->
  Parser (Maybe a)
switchParserHelper mkHelpFn opts cons name helpTxt = fmap cons <$> mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (OA.str >>= Phase.parseSwitch)
          ( mconcat
              [ opts,
                OA.long name,
                OA.metavar "(on | off)",
                OA.completeWith ["on", "off"],
                mkHelpFn helpTxt
              ]
          )

itemize :: NonEmpty String -> Mod OptionFields a
itemize =
  OA.helpDoc
    . Chunk.unChunk
    . fmap (<> Pretty.line)
    . itemizeHelper

-- | 'itemize' that does not append a trailing newline. Useful for the last
-- option in a group, as groups already start a newline.
itemizeNoLine :: NonEmpty String -> Mod OptionFields a
itemizeNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . itemizeHelper

itemizeHelper :: NonEmpty String -> Chunk Doc
itemizeHelper (intro :| ds) =
  Chunk.vcatChunks
    ( Chunk.paragraph intro
        : toChunk Pretty.softline
        : (toItem <$> ds)
    )
  where
    toItem d =
      fmap (Pretty.nest 2)
        . Chunk.paragraph
        $ ("- " <> d)

toChunk :: a -> Chunk a
toChunk = Chunk . Just
