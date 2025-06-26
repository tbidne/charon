<div align="center">

# charon

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/charon?include_prereleases&sort=semver)](https://github.com/tbidne/charon/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/charon/ci.yaml?branch=main)](https://github.com/tbidne/charon/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/charon?color=blue)](https://opensource.org/licenses/MIT)

![linux](https://img.shields.io/static/v1?label=&message=linux&logo=linux&logoColor=white&labelColor=2f353e&color=blue)
![osx](https://img.shields.io/static/v1?label=&message=osx&logo=apple&labelColor=2f353e&color=blue)
![windows](https://img.shields.io/static/v1?label=&message=windows&logo=windows&labelColor=2f353e&color=blue)

</div>

---

### Table of Contents

- [Introduction](#introduction)
  - [Usage](#usage)
- [Configuration](#configuration)
- [Commands](#commands)
  - [Delete Commands](#delete-commands)
    - [Delete](#delete)
    - [Permanent Delete](#permanent-delete)
    - [Empty](#empty)
  - [Restore Commands](#restore-commands)
    - [Restore](#restore)
  - [Information Commands](#information-commands)
    - [List](#list)
    - [Metadata](#metadata)
  - [Transform Commands](#transform-commands)
    - [Convert](#convert)
    - [Merge](#merge)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)

# Introduction

`charon` is a CLI tool for deleting files (like `rm`), but instead of permanently deleting files,
 moves them to a trash location (like Windows' recycle bin, or OSX's trash).

## Usage

```
Charon: A tool for deleting files to a trash directory.

Usage: charon [-c|--config (none|PATH)] [-b|--backend (cbor|fdo|json)]
              [--log-level (none|fatal|error|warn|info|debug)]
              [--log-size-mode (warn SIZE | delete SIZE)] [-t|--trash-home PATH]
              COMMAND

  Charon moves files to a trash directory, so they can later be restored or
  permanently deleted. It is intended as a safer alternative to rm. See
  github.com/tbidne/charon#readme for full documentation.

Available options:
  -c,--config (none|PATH)  Path to the toml config file. Can be the string
                           'none' -- in which case no toml config is used -- or
                           a path to the config file. If not specified then we
                           look in the XDG config directory e.g.
                           ~/.config/charon/config.toml

  -b,--backend (cbor|fdo|json)
                           Backend to use with charon. This option affects how
                           path metadata is stored. Options are:

                           - cbor: Space efficient, not inspectable.
                           - fdo: Compatible with FreeDesktop.org.
                           - json: Inspectable.

  --log-level (none|fatal|error|warn|info|debug)
                           The file level in which to log. Defaults to none.
                           Logs are written to the XDG state directory e.g.
                           ~/.local/state/charon.

  --log-size-mode (warn SIZE | delete SIZE)
                           Sets a threshold for the file log size, upon which we
                           either print a warning or delete the file, if it is
                           exceeded. The SIZE should include the value and units
                           e.g. 'warn 10 mb', 'warn 5 gigabytes', 'delete
                           20.5B'.

  -t,--trash-home PATH     Path to the trash directory. This overrides the toml
                           config, if it exists. If neither is given then we use
                           the XDG data directory e.g. ~/.local/share/charon.

  -h,--help                Show this help text

Delete Commands
  delete                   Moves the path(s) to the trash.

  d                        Alias for delete.

  perm-delete              Permanently deletes path(s) from the trash. Can be
                           run with explicit paths, wildcards, or --indices.

                           Examples:

                             Deleting explicit paths f1 f2 f3:
                             $ charon perm-delete f1 f2 f3

                             Wildcard search; matches foobar, xxxfooyyybar, etc:
                             $ charon perm-delete '*foo*bar'

                             Prints out trash index first, allows delete via numeric indices:
                             $ charon perm-delete --indices

  x                        Alias for perm-delete.

  empty                    Empties the trash.

  e                        Alias for empty.


Restore Commands
  restore                  Restores the trash path(s) to their original
                           location. Can be run with explicit paths, wildcards,
                           or --indices.

                           Examples:

                             Restoring explicit paths f1 f2 f3:
                             $ charon restore f1 f2 f3

                             Wildcard search; matches foobar, xxxfooyyybar, etc:
                             $ charon restore '*foo*bar'

                             Prints out trash index first, allows restore via numeric indices:
                             $ charon restore --indices

  r                        Alias for restore.


Information Commands
  list                     Lists all trash contents.

  l                        Alias for list.

  metadata                 Prints trash metadata.

  m                        Alias for metadata.


Transform Commands
  convert                  Converts the backend.

  merge                    Merges src (implicit or -t) trash home into dest.
                           Collisions will throw an error.

Version: 0.1 (bbdf7ac)
```

# Configuration

`charon` can be configured by either CLI args or a `toml` config file. A path to the config file can be given with the `-c` option. Otherwise we search in the XDG config e.g. `~/.config/charon/config.toml`. In general, if an option can be specified in both the config file and on the CLI (e.g. `--trash-home`), then the CLI takes priority.

See [config.toml](./examples/config.toml) for a description of the `toml` file.

# Commands

This section describes the possible commands, along with their specific options.

## Delete Commands

### Delete

**Usage:**

```
Usage: charon delete PATHS...

  Moves the path(s) to the trash.


Available options:
  -h,--help                Show this help text
```

**Examples**

```
# moves paths "foo", "bar", and "baz" to the trash
$ charon delete foo bar baz
```

### Permanent Delete

**Usage:**

```
Usage: charon perm-delete [--no-prompt] [-i|--indices] [PATHS...]

  Permanently deletes path(s) from the trash. Can be run with explicit paths,
  wildcards, or --indices.

  Examples:

    Deleting explicit paths f1 f2 f3:
    $ charon perm-delete f1 f2 f3

    Wildcard search; matches foobar, xxxfooyyybar, etc:
    $ charon perm-delete '*foo*bar'

    Prints out trash index first, allows delete via numeric indices:
    $ charon perm-delete --indices


Available options:
  --no-prompt              If enabled, will not ask before deleting path(s).

  -i,--indices             If active, allows selecting by numeric index instead
                           of trash name. Incompatible with explicit paths.

  -h,--help                Show this help text
```

**Examples**

```
# permanently deletes "foo", "bar", and "baz" from the trash directory
$ charon perm-delete foo bar baz

Type:      File
Name:      foo
Original:  /path/to/foo
Size:      0.00B
Created:   2023-02-24 14:32:01

Permanently delete (y/n)?
```

### Empty

**Usage:**

```
Usage: charon empty [--no-prompt]

  Empties the trash.


Available options:
  --no-prompt              If enabled, will not ask before emptying the trash.

  -h,--help                Show this help text
```

**Examples**

```
$ charon empty

Entries:      8
Total Files:  12
Log size:     144.80K
Size:         31.36K

Permanently delete all contents (y/n)?
```

## Restore Commands

### Restore

**Usage:**

```
Usage: charon restore [--force] [--no-prompt] [-i|--indices] [PATHS...]

  Restores the trash path(s) to their original location. Can be run with
  explicit paths, wildcards, or --indices.

  Examples:

    Restoring explicit paths f1 f2 f3:
    $ charon restore f1 f2 f3

    Wildcard search; matches foobar, xxxfooyyybar, etc:
    $ charon restore '*foo*bar'

    Prints out trash index first, allows restore via numeric indices:
    $ charon restore --indices


Available options:
  --force                  If enabled, will forcibly overwrite restored path(s).
                           Otherwise, collisions with existing paths will either
                           throw an error (with --no-prompt) or prompt the user
                           to decide.

  --no-prompt              If enabled, will not ask before restoring path(s).
                           Collisions with existing paths will either error or
                           overwrite, depending on --force.

  -i,--indices             If active, allows selecting by numeric index instead
                           of trash name. Incompatible with explicit paths.

  -h,--help                Show this help text
```

**Examples**

```
# deleting "foo" and "baz" first
$ charon delete foo baz

# restore "foo" and "baz" to their original locations
$ charon restore foo baz
```

## Information Commands

### List

**Usage:**

```
Usage: charon list [--color (t[rue] | f[alse] | d[etect])]
                   [--format (m[ulti] | s[ingle] | t[abular] | (ts|tabular-simple))]
                   [-n|--name-len (max|NAT)] [-o|--orig-len (max|NAT)]
                   [-s|--sort (name|size)] [-r|--reverse-sort]

  Lists all trash contents.


Available options:
  --color (t[rue] | f[alse] | d[etect])
                           Determines if we should color output. Multiline is
                           unaffected.

  --format (m[ulti] | s[ingle] | t[abular] | (ts|tabular-simple))
                           Formatting options.

                           - multi: Prints each entry across multiple lines.

                           - single: Compact, prints each entry across a single
                           lines

                           - tabular: The default. Prints a table that tries to
                           intelligently size the table based on available
                           terminal width and filename / original path lengths.

                           - tabular-simple: Simple table that does no resizing.
                           Prints the table with indices.

  -n,--name-len (max|NAT)  Sets the file name column length to either NAT
                           characters or longest file-name. Only affects the
                           'tabular' format.

  -o,--orig-len (max|NAT)  Sets the original-path column length to either NAT
                           characters or longest path. Only affects the
                           'tabular' format.

  -s,--sort (name|size)    How to sort the list. Defaults to name. Does not
                           affect 'single' style.

  -r,--reverse-sort        Sorts in the reverse order. Does not affect 'single'
                           style.

  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ charon delete foo bar baz

# list contents
$ charon list

Name | Original            | Type | Size    | Created
-----------------------------------------------------------------
bar  | /bar                | F    | 410.35G | 2022-10-28 15:33:18
baz  | /a/long/path/to/baz | F    | 45.61M  | 2022-10-28 15:33:18
foo  | /path/to/foo        | D    | 24.38B  | 2022-10-28 15:33:18
```

### Metadata

**Usage:**

```
Usage: charon metadata

  Prints trash metadata.


Available options:
  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ charon delete foo bar baz

# print metadata
$ charon metadata

Entries:      3
Total Files:  3
Log size:     4.89K
Size:         111.35M
```

## Transform Commands

### Convert

```
Usage: charon convert (-d|--dest (cbor|fdo))

  Converts the backend.


Available options:
  -d,--dest (cbor|fdo)     Backend to which we convert the current backend. See
                           --backend for more details

  -h,--help                Show this help text
```

**Examples**

```
# converting our trash info files from the cbor serialization to the
# FreeDesktop.org spec.
$ charon convert -b cbor -d fdo
```

### Merge

```
Usage: charon merge (-d|--dest PATH)

  Merges src (implicit or -t) trash home into dest. Collisions will throw an
  error.


Available options:
  -d,--dest PATH           Path to the dest trash directory.

  -h,--help                Show this help text
```

**Examples**

```
# merging default trash (~/.local/share/charon/ or toml file config) into FDO location
$ charon merge -d ~/.local/share/Trash
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 2.4+`](https://www.haskell.org/cabal/download.html)
* [`ghc 9.6 - 9.12`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)

* [`ghcup`](https://www.haskell.org/ghcup/)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

### Build Charon

Once you have `cabal` and `ghc`, `charon` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `charon` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `charon` is a flake, it be built as part of a nix expression. For instance, if you want to add `charon` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.charon.url = "github:tbidne/charon/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    charon.packages."${system}".default
  ];
}
```
