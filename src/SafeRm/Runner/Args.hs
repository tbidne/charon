{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module SafeRm.Runner.Args
  ( getArgs,
    Args (..),
    CommandArg (..),
    _DeleteArg,
    _DeletePermArg,
    _EmptyArg,
    _RestoreArg,
    _ListArg,
    _MetadataArg,
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
import Options.Applicative.Types (ArgPolicy (Intersperse))
import SafeRm.Data.Index (Sort, readSort)
import SafeRm.Data.Paths (PathI, PathIndex (OriginalPath, TrashHome, TrashName))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Prelude
import SafeRm.Runner.Config (CmdListCfg (MkCmdListCfg), ListFormatCfg, parseListFormat)
import SafeRm.Runner.FileSizeMode (FileSizeMode, parseFileSizeMode)
import SafeRm.Utils qualified as Utils

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

-- | Args representing a command. Analagous to SafeRm.Runner.Command,
-- though we maintain a separate type here as the actual command is derived
-- from a combination of arguments.
--
-- @since 0.1
data CommandArg
  = -- | Deletes a path.
    --
    -- @since 0.1
    DeleteArg !(UniqueSeq (PathI OriginalPath))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    DeletePermArg !Bool !(UniqueSeq (PathI TrashName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    EmptyArg !Bool
  | -- | Restores a path.
    --
    -- @since 0.1
    RestoreArg (UniqueSeq (PathI TrashName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    ListArg !CmdListCfg
  | -- | Prints trash metadata.
    --
    -- @since 0.1
    MetadataArg
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''CommandArg

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Path to toml config.
    --
    -- @since 0.1
    tomlConfigPath :: !TomlConfigPath,
    -- | Path to trash home.
    --
    -- @since 0.1
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | The file logging level.
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
    command :: !CommandArg
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
      infoProgDesc = Chunk desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Safe-rm: A tool for deleting files to a trash directory."
    footerTxt = Just $ fromString versNum
    desc =
      Just $
        mconcat
          [ "\nSafe-rm moves files to a trash directory, so they can later be ",
            "restored or permanently deleted. It is intended as a safer ",
            "alternative to rm. See github.com/tbidne/safe-rm#readme for ",
            "full documentation."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> configParser
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

configParser :: Parser TomlConfigPath
configParser =
  OA.option
    readTomlPath
    $ mconcat
      [ OA.value TomlDefault,
        OA.long "config",
        OA.short 'c',
        OA.metavar "(none|PATH)",
        OA.help helpTxt
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

commandParser :: Parser CommandArg
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
    delTxt = OA.progDesc "Moves the path(s) to the trash."
    permDelTxt = OA.progDesc "Permanently deletes path(s) from the trash."
    emptyTxt = OA.progDesc "Empties the trash."
    restoreTxt =
      OA.progDesc
        "Restores the trash path(s) to their original location."
    listTxt = OA.progDesc "Lists all trash contents and metadata."
    metadataTxt = OA.progDesc "Prints trash metadata."

    delParser = DeleteArg <$> pathsParser
    permDelParser = DeletePermArg <$> forceParser <*> pathsParser
    emptyParser = EmptyArg <$> forceParser
    restoreParser = RestoreArg <$> pathsParser
    listParser =
      fmap ListArg $
        MkCmdListCfg
          <$> listFormatParser
          <*> nameTruncParser
          <*> origTruncParser
          <*> sortParser
          <*> reverseSortParser
    metadataParser = pure MetadataArg

listFormatParser :: Parser (Maybe ListFormatCfg)
listFormatParser =
  A.optional $
    OA.option (OA.str >>= parseListFormat) $
      mconcat
        [ OA.long "format",
          OA.metavar "(a[uto] | t[abular] | m[ulti])",
          OA.help helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Determines the output format. The 'tabular' option prints each ",
          "trash entry on a single line, in tabular form. The 'multi' option ",
          "prints each entry across multiple lines. Finally, 'auto', the ",
          "default, has the same structure as 'tabular', except it attempts ",
          "to choose the best name/path column sizes automatically based on ",
          "the data and terminal width."
        ]

nameTruncParser :: Parser (Maybe Natural)
nameTruncParser = natParser fields
  where
    fields =
      mconcat
        [ OA.long "name-trunc",
          OA.short 'n',
          OA.metavar "NAT",
          OA.help $
            "Truncates the name to NAT chars. Only affects the 'tabular' "
              <> "format."
        ]

origTruncParser :: Parser (Maybe Natural)
origTruncParser = natParser fields
  where
    fields =
      mconcat
        [ OA.long "orig-trunc",
          OA.short 'o',
          OA.metavar "NAT",
          OA.help $
            "Truncates the original path to NAT chars. Only affects the 'tabular' "
              <> "format."
        ]

natParser :: Mod OptionFields Natural -> Parser (Maybe Natural)
natParser = A.optional . OA.option OA.auto

sortParser :: Parser (Maybe Sort)
sortParser =
  A.optional
    $ OA.option
      (OA.str >>= readSort)
    $ mconcat
      [ OA.long "sort",
        OA.short 's',
        OA.metavar "(name|size)",
        OA.help "How to sort the list. Defaults to name."
      ]

reverseSortParser :: Parser (Maybe Bool)
reverseSortParser =
  A.optional $
    OA.flag' True $
      mconcat
        [ OA.long "reverse-sort",
          OA.short 'r',
          OA.help helpTxt
        ]
  where
    helpTxt = "Sorts in the reverse order."

forceParser :: Parser Bool
forceParser =
  OA.switch $
    mconcat
      [ OA.long "force",
        OA.short 'f',
        OA.help helpTxt
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
        OA.help helpTxt
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
          OA.help $
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
            OA.help helpTxt,
            OA.metavar "<warn SIZE | delete SIZE>"
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
