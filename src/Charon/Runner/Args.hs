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
    DeleteParams (MkDeleteParams, paths, verbose),
    PermDeleteParams (MkPermDeleteParams, prompt, strategy, verbose),
    RestoreParams (MkRestoreParams, force, prompt, strategy, verbose),
  )
import Charon.Runner.Command.List
  ( ListCmd (MkListCmd),
    ListFormatPhase1 (MkListFormatPhase1),
    ListFormatStyle,
    parseListFormat,
  )
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LogLevelConfig,
    LoggingConfig (MkLoggingConfig, logLevel, logSizeMode),
  )
import Charon.Runner.Config qualified as Config
import Charon.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import Charon.Runner.Phase (ConfigPhase (ConfigPhaseArgs))
import Charon.Runner.WithDisabled (WithDisabled (Disabled, With, Without))
import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version (showVersion)
import Effects.Optparse (osPath)
import FileSystem.OsString qualified as OsString
import Options.Applicative
  ( CommandFields,
    FlagFields,
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
        OA.metavar "(cbor|fdo|json)",
        OA.helpDoc helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ intro,
          Just Pretty.hardline,
          cbor,
          fdo,
          js,
          Just Pretty.hardline
        ]

    intro =
      toMDoc "Backend to use with charon. This option affects how path metadata is stored. Options are: "
    cbor = Just Pretty.hardline <> toMDoc "- cbor: Space efficient, not inspectable."
    fdo = Just Pretty.hardline <> toMDoc "- fdo: Compatible with FreeDesktop.org."
    js = Just Pretty.hardline <> toMDoc "- json: Inspectable."

backendDestParser :: Parser Backend
backendDestParser =
  OA.option (OA.str >>= parseBackend)
    $ mconcat
      [ OA.long "dest",
        OA.short 'd',
        OA.metavar "(cbor|fdo|json)",
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
        OA.metavar "(none|PATH)",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the toml config file. Can be the string 'none' -- in which ",
          "case no toml config is used -- or a path to the config file. If ",
          "not specified then we look in the XDG config directory ",
          "e.g. ~/.config/charon/config.toml"
        ]
    readTomlPath = do
      p <- osPath
      if p == [osp|none|]
        then pure TomlNone
        else pure $ TomlPath p

commandParser :: Parser (Command ConfigPhaseArgs)
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "delete" delParser delTxt,
          mkCommand "d" delParser (mkCmdDescStr "Alias for delete."),
          mkCommand "perm-delete" permDelParser permDelTxt,
          mkCommand "x" permDelParser (mkCmdDescStr "Alias for perm-delete."),
          mkCommand "empty" emptyParser emptyTxt,
          mkCommand "e" emptyParser (mkCmdDescStr "Alias for empty."),
          OA.commandGroup "Delete Commands"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "restore" restoreParser restoreTxt,
            mkCommand "r" restoreParser (mkCmdDescStr "Alias for restore."),
            OA.commandGroup "Restore Commands",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "list" listParser listTxt,
            mkCommand "l" listParser (mkCmdDescStr "Alias for list."),
            mkCommand "metadata" metadataParser metadataTxt,
            mkCommand "m" metadataParser (mkCmdDescStr "Alias for metadata."),
            OA.commandGroup "Information Commands",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "convert" convertParser convertTxt,
            mkCommand "merge" mergeParser mergeTxt,
            OA.commandGroup "Transform Commands",
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
    emptyTxt = mkCmdDescStr "Empties the trash."
    restoreTxt =
      mkCmdDesc
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
    metadataTxt = mkCmdDescStr "Prints trash metadata."
    convertTxt = mkCmdDescStr "Converts the backend."
    mergeTxt = mkCmdDescNoLine "Merges src (implicit or -t) trash home into dest. Collisions will throw an error."

    mkExample :: [String] -> Chunk Doc
    mkExample =
      Chunk.vcatChunks
        . fmap (fmap (Pretty.indent 2) . Chunk.stringChunk)

    delParser =
      Delete <$> do
        paths <- pathsParser
        verbose <- verboseParser "Lists deleted paths."
        pure
          $ MkDeleteParams
            { paths,
              verbose
            }
    permDelParser =
      PermDelete <$> do
        prompt <- promptParser "Prompts before deleting path(s). This is the default."
        indices <- indicesParser
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
        <$> promptParser "Prompts before emptying the trash. This is the default."
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
          "(with --no-prompt) or prompt the user to decide."
        ]
    restorePromptTxt = "Prompts before restoring path(s). This is the default."
    listParser =
      fmap List
        $ MkListCmd
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

indicesParser :: Parser (WithDisabled ())
indicesParser = withDisabledParser mainParser "indices"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.short 'i',
              OA.long "indices",
              mkHelp
                $ mconcat
                  [ "Allows selecting by numeric index instead of trash name. ",
                    "Incompatible with explicit paths. The prompt can be exited ",
                    "via 'exit', 'quit', or ':q'."
                  ]
            ]
        )
    mainParser = do
      b <- switchParser
      pure
        $ if b
          then Just ()
          else Nothing

listFormatStyleParser :: Parser (Maybe ListFormatStyle)
listFormatStyleParser =
  A.optional
    $ OA.option (OA.str >>= parseListFormat)
    $ mconcat
      [ OA.long "format",
        OA.metavar "FMT",
        OA.helpDoc helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ intro,
          Just Pretty.hardline,
          multi,
          single,
          tabular,
          tabularSimple,
          Just Pretty.hardline
        ]
    intro = toMDoc "Formatting options."
    tabular =
      mconcat
        [ Just Pretty.hardline,
          toMDoc
            $ mconcat
              [ "- (t|tabular): The default. Prints a table that tries to ",
                "intelligently size the table based on available terminal ",
                "width and filename / original path lengths."
              ]
        ]
    tabularSimple =
      mconcat
        [ Just Pretty.hardline,
          toMDoc
            $ mconcat
              [ "- (ts|tabular-simple): Simple table that does no resizing. Prints ",
                "the table with indices."
              ]
        ]
    multi =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (m|multi): Prints each entry across multiple lines."
        ]
    single =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (s|single): Compact, prints each entry across a single lines."
        ]

toMDoc :: String -> Maybe Doc
toMDoc = Chunk.unChunk . Chunk.paragraph

nameTruncParser :: Parser (Maybe ColFormat)
nameTruncParser = colParser PathData.formatFileNameLenMin fields
  where
    fields =
      mconcat
        [ OA.long "name-len",
          OA.short 'n',
          OA.metavar "(max|NAT)",
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
          OA.metavar "(max|NAT)",
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
        OA.helpDoc helpTxt
      ]
  where
    readColoring =
      OA.str >>= \case
        "t" -> pure ColoringOn
        "true" -> pure ColoringOn
        "f" -> pure ColoringOff
        "false" -> pure ColoringOff
        "d" -> pure ColoringDetect
        "detect" -> pure ColoringDetect
        bad -> fail $ "Unexpected --coloring: " ++ bad

    helpTxt =
      mconcat
        [ intro,
          Just Pretty.hardline,
          true,
          false,
          detect,
          Just Pretty.hardline
        ]
    intro = toMDoc "Coloring options."
    true =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (t|true): On."
        ]
    false =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (f|false): Off."
        ]
    detect =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (d|detect): On if supported."
        ]

sortParser :: Parser (Maybe Sort)
sortParser =
  A.optional
    $ OA.option
      (OA.str >>= readSort)
    $ mconcat
      [ OA.long "sort",
        OA.short 's',
        OA.metavar "(name|size)",
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

forceParser :: String -> Parser (WithDisabled ())
forceParser helpTxt = withDisabledParser mainParser "force"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "force",
              mkHelp helpTxt
            ]
        )
    mainParser = do
      b <- switchParser
      pure
        $ if b
          then Just ()
          else Nothing

promptParser :: String -> Parser (WithDisabled ())
promptParser helpTxt = withDisabledParser mainParser "prompt"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "prompt",
              mkHelp helpTxt
            ]
        )
    mainParser = do
      b <- switchParser
      pure
        $ if b
          then Just ()
          else Nothing

verboseParser :: String -> Parser (WithDisabled ())
verboseParser helpTxt = withDisabledParser mainParser "verbose"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "verbose",
              OA.short 'v',
              mkHelp helpTxt
            ]
        )
    mainParser = do
      b <- switchParser
      pure
        $ if b
          then Just ()
          else Nothing

trashParser :: Parser (Maybe (RawPathI TrashHome))
trashParser =
  A.optional
    $ OA.option
      (fmap MkRawPathI osPath)
    $ mconcat
      [ OA.long "trash-home",
        OA.short 't',
        OA.metavar "PATH",
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
        mkHelp
          $ mconcat
            [ "The file level in which to log. Defaults to none. Logs are ",
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
            OA.metavar "(warn SIZE|delete SIZE)"
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

mkCmdDescStr :: String -> InfoMod a
mkCmdDescStr = mkCmdDesc . Chunk.paragraph

mkCmdDesc :: Chunk Doc -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk

-- For the last command, so we do not append two lines (there is an automatic
-- one at the end).
mkCmdDescNoLine :: String -> InfoMod a
mkCmdDescNoLine =
  OA.progDescDoc
    . Chunk.unChunk
    . Chunk.paragraph

-- | Adds a '--no-x' switch to the parser.
withDisabledParser ::
  -- | Main parser.
  Parser (Maybe a) ->
  -- | Name for this option, to be used in disabled switch name.
  String ->
  Parser (WithDisabled a)
withDisabledParser mainParser name =
  withDisabledParserOpts opts mainParser name
  where
    helpTxt = "Disables --" ++ name ++ "."
    opts = mkHelp helpTxt

-- | Like 'withDisabledParser', except it also takes an arg for the disabled
-- switch options.
withDisabledParserOpts ::
  -- | Disabled switch options.
  Mod FlagFields Bool ->
  -- | Main parser
  Parser (Maybe a) ->
  -- | Name for this option, to be used in disabled switch name.
  String ->
  Parser (WithDisabled a)
withDisabledParserOpts disabledOpts mainParser name = do
  mx <- mainParser
  y <- noParser
  pure
    $ if y
      then Disabled
      else maybe Without With mx
  where
    noParser =
      OA.flag
        False
        True
        ( mconcat
            [ OA.long $ "no-" ++ name,
              OA.hidden,
              disabledOpts
            ]
        )
