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
import Charon.Data.PathData.Formatting (ColFormat (ColFormatFixed, ColFormatMax), Coloring (ColoringDetect, ColoringOff, ColoringOn))
import Charon.Data.PathData.Formatting qualified as PathData
import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Data.UniqueSeqNE qualified as UniqueSeqNE
import Charon.Prelude
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
    CommandP1,
  )
import Charon.Runner.Command.List
  ( ListCmd (MkListCmd),
    ListFormatPhase1 (MkListFormatPhase1),
    ListFormatStyle,
    parseListFormat,
  )
import Charon.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import Charon.Utils qualified as Utils
import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version (Version (versionBranch))
import Effects.Optparse (osPath)
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
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_charon qualified as Paths
import Text.Read qualified as TR

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
  { -- | Path to toml config.
    tomlConfigPath :: TomlConfigPath,
    -- | Backend to use.
    backend :: Maybe Backend,
    -- | Path to trash home.
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | The file logging level. The double Maybe is so we distinguish between
    -- unspecified (Nothing) and explicitly disabled (Just Nothing).
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | Whether to warn/delete for large log files.
    logSizeMode :: Maybe FileSizeMode,
    -- | Command to run.
    command :: CommandP1
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
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Charon moves files to a trash directory, so they can later be ",
            "restored or permanently deleted. It is intended as a safer ",
            "alternative to rm. See github.com/tbidne/charon#readme for ",
            "full documentation."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> configParser
    <*> backendParser
    <*> trashParser
    <*> logLevelParser
    <*> logSizeModeParser
    <**> version
    <**> OA.helper
    <*> commandParser

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version")

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

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

    toMDoc = Chunk.unChunk . Chunk.paragraph

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

commandParser :: Parser CommandP1
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "delete" delParser delTxt,
          mkCommand "d" delParser (mkCmdDesc "Alias for delete."),
          mkCommand "perm-delete" permDelParser permDelTxt,
          mkCommand "x" permDelParser (mkCmdDesc "Alias for perm-delete."),
          mkCommand "empty" emptyParser emptyTxt,
          mkCommand "e" emptyParser (mkCmdDesc "Alias for empty."),
          OA.commandGroup "Delete Commands"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "restore" restoreParser restoreTxt,
            mkCommand "r" restoreParser (mkCmdDesc "Alias for restore."),
            OA.commandGroup "Restore Commands",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "list" listParser listTxt,
            mkCommand "l" listParser (mkCmdDesc "Alias for list."),
            mkCommand "metadata" metadataParser metadataTxt,
            mkCommand "m" metadataParser (mkCmdDesc "Alias for metadata."),
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
    delTxt = mkCmdDesc "Moves the path(s) to the trash."
    permDelTxt =
      mkCmdDesc
        $ mconcat
          [ "Permanently deletes path(s) from the trash. Can use wildcards ",
            "to match trash paths e.g. '*foo*bar' matches foobar, xxxfooyyybar, ",
            "etc. To match a filename with a literal * not representing a ",
            " wildcard -- e.g. '*foo' -- the * must be escaped (charon perm-delete '\\*foo')."
          ]
    emptyTxt = mkCmdDesc "Empties the trash."
    restoreTxt =
      mkCmdDesc
        $ mconcat
          [ "Restores the trash path(s) to their original location. Can use ",
            "wildcards to match trash paths e.g. '*foo*bar' matches foobar, ",
            "xxxfooyyybar, etc. To match a filename with a literal * not representing a ",
            " wildcard -- e.g. '*foo' -- the * must be escaped (charon restore '\\*foo')."
          ]
    listTxt = mkCmdDesc "Lists all trash contents."
    metadataTxt = mkCmdDesc "Prints trash metadata."
    convertTxt = mkCmdDesc "Converts the backend."
    mergeTxt = mkCmdDescNoLine "Merges src (implicit or -t) trash home into dest. Collisions will throw an error."

    delParser = Delete <$> pathsParser
    permDelParser = PermDelete <$> forceParser <*> pathsParser
    emptyParser = Empty <$> forceParser
    restoreParser = Restore <$> pathsParser
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

listFormatStyleParser :: Parser (Maybe ListFormatStyle)
listFormatStyleParser =
  A.optional
    $ OA.option (OA.str >>= parseListFormat)
    $ mconcat
      [ OA.long "format",
        OA.metavar "(m[ulti] | s[ingle] | t[abular])",
        OA.helpDoc helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ intro,
          Just Pretty.hardline,
          multi,
          Just Pretty.hardline,
          single,
          Just Pretty.hardline,
          tabular,
          Just Pretty.hardline
        ]
    intro = toMDoc "Formatting options."
    tabular = Just Pretty.hardline <> toMDoc "- tabular: The default. Prints a table that tries to intelligently size the table based on available terminal width and filename / original path lengths."
    multi = Just Pretty.hardline <> toMDoc "- multi: Prints each entry across multiple lines."
    single = Just Pretty.hardline <> toMDoc "- single: Compact, prints each entry across a single lines"
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
        OA.metavar "(t[rue] | f[alse] | d[etect])",
        mkHelp
          $ mconcat
            [ "Determines if we should color output. Multiline is unaffected."
            ]
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

sortParser :: Parser (Maybe Sort)
sortParser =
  A.optional
    $ OA.option
      (OA.str >>= readSort)
    $ mconcat
      [ OA.long "sort",
        OA.short 's',
        OA.metavar "(name|size)",
        mkHelp "How to sort the list. Defaults to name. Does not affect 'single' style."
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
    helpTxt = "Sorts in the reverse order. Does not affect 'single' style."

forceParser :: Parser Bool
forceParser =
  OA.switch
    $ mconcat
      [ OA.long "force",
        OA.short 'f',
        mkHelp helpTxt
      ]
  where
    helpTxt = "If enabled, will not ask before deleting path(s)."

trashParser :: Parser (Maybe (PathI TrashHome))
trashParser =
  A.optional
    $ OA.option
      (fmap MkPathI osPath)
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

trashDestParser :: Parser (PathI TrashHome)
trashDestParser =
  OA.option
    (fmap MkPathI osPath)
    $ mconcat
      [ OA.long "dest",
        OA.short 'd',
        OA.metavar "PATH",
        mkHelp helpTxt
      ]
  where
    helpTxt = "Path to the dest trash directory."

logLevelParser :: Parser (Maybe (Maybe LogLevel))
logLevelParser =
  A.optional
    $ OA.option (OA.str >>= Utils.readLogLevel)
    $ mconcat
      [ OA.long "log-level",
        OA.metavar Utils.logLevelStrings,
        mkHelp
          $ mconcat
            [ "The file level in which to log. Defaults to none. Logs are ",
              "written to the XDG state directory e.g. ~/.local/state/charon."
            ]
      ]

pathsParser :: Parser (UniqueSeqNE (PathI i))
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
    <$> OA.some (OA.argument (fmap MkPathI osPath) (OA.metavar "PATHS..."))

logSizeModeParser :: Parser (Maybe FileSizeMode)
logSizeModeParser =
  OA.optional
    $ OA.option
      readFileSize
      ( mconcat
          [ OA.long "log-size-mode",
            mkHelp helpTxt,
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

mkCmdDesc :: String -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

-- For the last command, so we do not append two lines (there is an automatic
-- one at the end).
mkCmdDescNoLine :: String -> InfoMod a
mkCmdDescNoLine =
  OA.progDescDoc
    . Chunk.unChunk
    . Chunk.paragraph
