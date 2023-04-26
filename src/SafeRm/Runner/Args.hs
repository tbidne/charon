{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module SafeRm.Runner.Args
  ( getArgs,
    Args (..),
    TomlConfigPath (..),
    _TomlNone,
    _TomlDefault,
    _TomlPath,
  )
where

import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
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
import SafeRm.Data.Backend (Backend (..), parseBackend)
import SafeRm.Data.Index (Sort, readSort)
import SafeRm.Data.PathData.Formatting (ColFormat (..))
import SafeRm.Data.PathData.Formatting qualified as PathData
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Prelude
import SafeRm.Runner.Command (Command (..), CommandP1)
import SafeRm.Runner.Command.List
  ( ListCmd (MkListCmd),
    ListFormatPhase1 (MkListFormatPhase1),
    ListFormatStyle,
    parseListFormat,
  )
import SafeRm.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import SafeRm.Utils qualified as Utils
import Text.Read qualified as TR

-- | Toml path config.
--
-- @since 0.1
data TomlConfigPath
  = -- | Do not use any Toml config.
    --
    -- @since 0.1
    TomlNone
  | -- | Attempts to read the Toml file at the default path.
    --
    -- @since 0.1
    TomlDefault
  | -- | Path to Toml file.
    --
    -- @since 0.1
    TomlPath !FilePath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makePrisms ''TomlConfigPath

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Path to toml config.
    --
    -- @since 0.1
    tomlConfigPath :: !TomlConfigPath,
    -- | Backend to use.
    backend :: !(Maybe Backend),
    -- | Path to trash home.
    --
    -- @since 0.1
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | The file logging level. The double Maybe is so we distinguish between
    -- unspecified (Nothing) and explicitly disabled (Just Nothing).
    --
    -- @since 0.1
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | Whether to warn/delete for large log files.
    --
    -- @since 0.1
    logSizeMode :: !(Maybe FileSizeMode),
    -- | Command to run.
    --
    -- @since 0.1
    command :: !CommandP1
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Args

-- | Retrieves CLI args.
--
-- @since 0.1
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
    headerTxt = Just "Safe-rm: A tool for deleting files to a trash directory."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph $
        mconcat
          [ "Safe-rm moves files to a trash directory, so they can later be ",
            "restored or permanently deleted. It is intended as a safer ",
            "alternative to rm. See github.com/tbidne/safe-rm#readme for ",
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
    <*> commandParser
    <**> OA.helper
    <**> version

version :: Parser (a -> a)
version = OA.infoOption txt (OA.long "version")
  where
    txt =
      L.intercalate
        "\n"
        [ "SafeRm",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: String
versNum = "Version: " <> $$(PV.packageVersionStringTH "safe-rm.cabal")

backendParser :: Parser (Maybe Backend)
backendParser =
  OA.optional $
    OA.option (OA.str >>= parseBackend) $
      mconcat
        [ OA.long "backend",
          OA.short 'b',
          OA.metavar "(default|fdo)",
          mkHelp helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Backend to use with safe-rm. This option affects how path metadata ",
          "is stored. The fdo option is compatible with the FreeDesktop.org ",
          "trash specification file format. Defaults to 'default', which ",
          "captures more information when a path is moved to the trash, at the ",
          "expense of compatibility."
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
          "e.g. ~/.config/safe-rm/config.toml"
        ]
    readTomlPath =
      OA.str >>= \case
        "none" -> pure TomlNone
        path -> pure $ TomlPath path

commandParser :: Parser CommandP1
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "d" delParser delTxt,
          mkCommand "x" permDelParser permDelTxt,
          mkCommand "e" emptyParser emptyTxt,
          OA.commandGroup "Delete Commands"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "r" restoreParser restoreTxt,
            OA.commandGroup "Restore Commands",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "l" listParser listTxt,
            mkCommand "m" metadataParser metadataTxt,
            OA.commandGroup "Information Commands",
            OA.hidden
          ]
      )
  where
    delTxt = mkCmdDesc "Moves the path(s) to the trash."
    permDelTxt =
      mkCmdDesc $
        mconcat
          [ "Permanently deletes path(s) from the trash. Can use wildcards ",
            "to match trash paths e.g. '*foo*bar' matches foobar, xxxfooyyybar, ",
            "etc. To match a filename with a literal * not representing a ",
            " wildcard -- e.g. '*foo' -- the * must be escaped (sr x '\\*foo')."
          ]
    emptyTxt = mkCmdDesc "Empties the trash."
    restoreTxt =
      mkCmdDesc $
        mconcat
          [ "Restores the trash path(s) to their original location. Can use ",
            "wildcards to match trash paths e.g. '*foo*bar' matches foobar, ",
            "xxxfooyyybar, etc. To match a filename with a literal * not representing a ",
            " wildcard -- e.g. '*foo' -- the * must be escaped (sr r '\\*foo')."
          ]
    listTxt = mkCmdDesc "Lists all trash contents."
    metadataTxt = mkCmdDesc "Prints trash metadata."

    delParser = Delete <$> pathsParser
    permDelParser = DeletePerm <$> forceParser <*> pathsParser
    emptyParser = Empty <$> forceParser
    restoreParser = Restore <$> pathsParser
    listParser =
      fmap List $
        MkListCmd
          <$> ( MkListFormatPhase1
                  <$> listFormatStyleParser
                  <*> nameTruncParser
                  <*> origTruncParser
              )
          <*> sortParser
          <*> reverseSortParser
    metadataParser = pure Metadata

listFormatStyleParser :: Parser (Maybe ListFormatStyle)
listFormatStyleParser =
  A.optional $
    OA.option (OA.str >>= parseListFormat) $
      mconcat
        [ OA.long "format",
          OA.metavar "(t[abular] | m[ulti])",
          mkHelp helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Determines the output format. The 'multi' option prints each ",
          "entry across multiple lines. The default 'tabular' option prints ",
          "each trash entry on a single line, in a table. By default, ",
          "tabular tries to intelligently size the table based on the ",
          "available terminal width and filename / original path lengths. ",
          "The behavior can be overridden via --name-len and --orig-len. ",
          "Note that this can lead to word-wrapping."
        ]

nameTruncParser :: Parser (Maybe ColFormat)
nameTruncParser = colParser PathData.formatFileNameLenMin fields
  where
    fields =
      mconcat
        [ OA.long "name-len",
          OA.short 'n',
          OA.metavar "(max|NAT)",
          mkHelp $
            mconcat
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
          mkHelp $
            mconcat
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
            fail $
              mconcat
                [ "Unrecognized col-format. Should either be 'max' or a positive ",
                  "integer < ",
                  show minLen,
                  ". Received: ",
                  other
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
  A.optional $
    OA.flag' True $
      mconcat
        [ OA.long "reverse-sort",
          OA.short 'r',
          mkHelp helpTxt
        ]
  where
    helpTxt = "Sorts in the reverse order."

forceParser :: Parser Bool
forceParser =
  OA.switch $
    mconcat
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
      OA.str
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
          "e.g. ~/.local/share/safe-rm."
        ]

logLevelParser :: Parser (Maybe (Maybe LogLevel))
logLevelParser =
  A.optional $
    OA.option (OA.str >>= Utils.readLogLevel) $
      mconcat
        [ OA.long "log-level",
          OA.metavar Utils.logLevelStrings,
          mkHelp $
            mconcat
              [ "The file level in which to log. Defaults to none. Logs are ",
                "written to the XDG state directory e.g. ~/.local/state/safe-rm."
              ]
        ]

pathsParser :: (Hashable a, IsString a) => Parser (UniqueSeq a)
pathsParser =
  -- NOTE: _should_ be safe because OA.some only succeeds for non-zero input.
  -- We do this rather than using NonEmpty's some1 because otherwise the CLI
  -- help metavar is duplicated i.e. "PATHS... [PATHS...]".
  fromFoldable . unsafeNE
    <$> OA.some (OA.argument OA.str (OA.metavar "PATHS..."))

logSizeModeParser :: Parser (Maybe FileSizeMode)
logSizeModeParser =
  OA.optional $
    OA.option
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
