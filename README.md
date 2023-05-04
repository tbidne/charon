<div align="center">

# safe-rm

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/safe-rm?include_prereleases&sort=semver)](https://github.com/tbidne/safe-rm/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/safe-rm?color=blue)](https://opensource.org/licenses/MIT)

![linux](https://img.shields.io/static/v1?label=&message=linux&logo=linux&logoColor=white&labelColor=2f353e&color=blue)
![osx](https://img.shields.io/static/v1?label=&message=osx&logo=apple&labelColor=2f353e&color=blue)
![windows](https://img.shields.io/static/v1?label=&message=windows&logo=windows&labelColor=2f353e&color=blue)

[![nix](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/nix.yaml?branch=main&label=nix&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/nix.yaml)
[![cabal](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/cabal.yaml?branch=main&label=cabal&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/cabal.yaml)
[![style](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/style.yaml?branch=main&label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/style.yaml)

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

`safe-rm` is a CLI tool for deleting files (like `rm`), but instead of permanently deleting files,
 moves them to a trash location (like Windows' recycle bin, or OSX's trash).

## Usage

```
Safe-rm: A tool for deleting files to a trash directory.

Usage: safe-rm [-c|--config (none|PATH)] [-b|--backend (cbor|fdo)]
               [-t|--trash-home PATH] [--log-level (none|error|warn|info|debug)]
               [--log-size-mode (warn SIZE | delete SIZE)] [--version] COMMAND

  Safe-rm moves files to a trash directory, so they can later be restored or
  permanently deleted. It is intended as a safer alternative to rm. See
  github.com/tbidne/safe-rm#readme for full documentation.

Available options:
  -c,--config (none|PATH)  Path to the toml config file. Can be the string
                           'none' -- in which case no toml config is used -- or
                           a path to the config file. If not specified then we
                           look in the XDG config directory e.g.
                           ~/.config/safe-rm/config.toml

  -b,--backend (cbor|fdo)  Backend to use with safe-rm. This option affects how
                           path metadata is stored. The fdo option is compatible
                           with the FreeDesktop.org trash specification file
                           format. Defaults to 'cbor'.

  -t,--trash-home PATH     Path to the trash directory. This overrides the toml
                           config, if it exists. If neither is given then we use
                           the XDG data directory e.g. ~/.local/share/safe-rm.

  --log-level (none|error|warn|info|debug)
                           The file level in which to log. Defaults to none.
                           Logs are written to the XDG state directory e.g.
                           ~/.local/state/safe-rm.

  --log-size-mode (warn SIZE | delete SIZE)
                           Sets a threshold for the file log size, upon which we
                           either print a warning or delete the file, if it is
                           exceeded. The SIZE should include the value and units
                           e.g. 'warn 10 mb', 'warn 5 gigabytes', 'delete
                           20.5B'.

  -h,--help                Show this help text

Delete Commands
  delete                   Moves the path(s) to the trash.

  d                        Alias for delete.

  perm-delete              Permanently deletes path(s) from the trash. Can use
                           wildcards to match trash paths e.g. '*foo*bar'
                           matches foobar, xxxfooyyybar, etc. To match a
                           filename with a literal * not representing a wildcard
                           -- e.g. '*foo' -- the * must be escaped (safe-rm
                           perm-delete '\*foo').

  x                        Alias for perm-delete.

  empty                    Empties the trash.

  e                        Alias for empty.


Restore Commands
  restore                  Restores the trash path(s) to their original
                           location. Can use wildcards to match trash paths e.g.
                           '*foo*bar' matches foobar, xxxfooyyybar, etc. To
                           match a filename with a literal * not representing a
                           wildcard -- e.g. '*foo' -- the * must be escaped
                           (safe-rm restore '\*foo').

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


Version: 0.1
```

# Configuration

`safe-rm` can be configured by either CLI args or a `toml` config file. A path to the config file can be given with the `-c` option. Otherwise we search in the XDG config e.g. `~/.config/safe-rm/config.toml`. In general, if an option can be specified in both the config file and on the CLI (e.g. `--trash-home`), then the CLI takes priority.

See [config.toml](./examples/config.toml) for a description of the `toml` file.

# Commands

This section describes the possible commands, along with their specific options.

## Delete Commands

### Delete

**Usage:**

```
Usage: safe-rm delete PATHS...

  Moves the path(s) to the trash.


Available options:
  -h,--help                Show this help text
```

**Examples**

```
# moves paths "foo", "bar", and "baz" to the trash
$ safe-rm delete foo bar baz
```

### Permanent Delete

**Usage:**

```
Usage: safe-rm perm-delete [-f|--force] PATHS...

  Permanently deletes path(s) from the trash. Can use wildcards to match trash
  paths e.g. '*foo*bar' matches foobar, xxxfooyyybar, etc. To match a filename
  with a literal * not representing a wildcard -- e.g. '*foo' -- the * must be
  escaped (safe-rm perm-delete '\*foo').


Available options:
  -f,--force               If enabled, will not ask before deleting path(s).

  -h,--help                Show this help text
```

**Examples**

```
# permanently deletes "foo", "bar", and "baz" from the trash directory
$ safe-rm perm-delete foo bar baz

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
Usage: safe-rm empty [-f|--force]

  Empties the trash.


Available options:
  -f,--force               If enabled, will not ask before deleting path(s).

  -h,--help                Show this help text
```

**Examples**

```
$ safe-rm empty

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
Usage: safe-rm restore PATHS...

  Restores the trash path(s) to their original location. Can use wildcards to
  match trash paths e.g. '*foo*bar' matches foobar, xxxfooyyybar, etc. To match
  a filename with a literal * not representing a wildcard -- e.g. '*foo' -- the
  * must be escaped (safe-rm restore '\*foo').


Available options:
  -h,--help                Show this help text
```

**Examples**

```
# deleting "foo" and "baz" first
$ safe-rm delete foo baz

# restore "foo" and "baz" to their original locations
$ safe-rm restore foo baz
```

## Information Commands

### List

**Usage:**

```
Usage: safe-rm list [--format (t[abular] | m[ulti])] [-n|--name-len (max|NAT)]
                    [-o|--orig-len (max|NAT)] [-s|--sort (name|size)]
                    [-r|--reverse-sort]

  Lists all trash contents.


Available options:
  --format (t[abular] | m[ulti])
                           Determines the output format. The 'multi' option
                           prints each entry across multiple lines. The default
                           'tabular' option prints each trash entry on a single
                           line, in a table. By default, tabular tries to
                           intelligently size the table based on the available
                           terminal width and filename / original path lengths.
                           The behavior can be overridden via --name-len and
                           --orig-len. Note that this can lead to word-wrapping.

  -n,--name-len (max|NAT)  Sets the file name column length to either NAT
                           characters or longest file-name. Only affects the
                           'tabular' format.

  -o,--orig-len (max|NAT)  Sets the original-path column length to either NAT
                           characters or longest path. Only affects the
                           'tabular' format.

  -s,--sort (name|size)    How to sort the list. Defaults to name.

  -r,--reverse-sort        Sorts in the reverse order.

  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ safe-rm delete foo bar baz

# list contents
$ safe-rm list

Name | Original            | Type | Size    | Created
-----------------------------------------------------------------
bar  | /bar                | F    | 410.35G | 2022-10-28 15:33:18
baz  | /a/long/path/to/baz | F    | 45.61M  | 2022-10-28 15:33:18
foo  | /path/to/foo        | D    | 24.38B  | 2022-10-28 15:33:18
```

### Metadata

**Usage:**

```
Usage: safe-rm metadata

  Prints trash metadata.


Available options:
  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ safe-rm delete foo bar baz

# print metadata
$ safe-rm metadata

Entries:      3
Total Files:  3
Log size:     4.89K
Size:         111.35M
```

## Transform Commands

### Convert

```
Usage: safe-rm convert (-d|--dest (cbor|fdo))

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
$ safe-rm convert -b cbor -d fdo
```

### Merge

```
Usage: safe-rm merge (-d|--dest PATH)

  Merges src (implicit or -t) trash home into dest. Collisions will throw an
  error.


Available options:
  -d,--dest PATH           Path to the dest trash directory.

  -h,--help                Show this help text
```

**Examples**

```
# merging default trash (~/.local/share/safe-rm/ or toml file config) into FDO location
$ safe-rm merge -d ~/.local/share/Trash
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`ghcup`](https://www.haskell.org/ghcup/)

Using `ghcup`, install `cabal 2.4+` and `ghc 9.4`.

### Build Safe-rm

Once you have `cabal` and `ghc`, `safe-rm` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `safe-rm` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `safe-rm` is a flake, it be built as part of a nix expression. For instance, if you want to add `safe-rm` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.safe-rm.url = "github:tbidne/safe-rm/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    safe-rm.packages."${system}".default
  ];
}
```