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
- [Installation](#installation)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)

# Introduction

`charon` is a CLI tool for deleting files (like `rm`), but instead of permanently deleting files,
 moves them to a trash location (like Windows' recycle bin, or OSX's trash).

# Configuration

`charon` can be configured by either CLI args or a `toml` config file. A path to the config file can be given with the `-c` option. Otherwise we search in the XDG config e.g. `~/.config/charon/config.toml`. In general, if an option can be specified in both the config file and on the CLI (e.g. `--trash-home`), then the CLI takes priority.

See [config.toml](./examples/config.toml) for a description of the `toml` file.

# Commands

This section describes the possible commands, along with their specific options.

## Delete Commands

### Delete

**Usage:**

```
Usage: charon delete PATHS... [--prompt] [-v|--verbose]

  Moves the path(s) to the trash.


Available options:
  --prompt                 Prompts before deleting path(s). Not the default.

  --no-prompt              Disables --prompt.

  -v,--verbose             Lists deleted paths.

  --no-verbose             Disables --verbose.

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
Usage: charon perm-delete [--prompt] [-i|--indices] [-v|--verbose] [PATHS...]

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
  --prompt                 Prompts before deleting path(s). This is the default.

  --no-prompt              Disables --prompt.

  -i,--indices             Allows selecting by numeric index instead of trash
                           name. Incompatible with explicit paths. The prompt
                           can be exited via 'exit', 'quit', or ':q'.

  --no-indices             Disables --indices.

  -v,--verbose             Lists deleted paths.

  --no-verbose             Disables --verbose.

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

# permanent delete via indices
$ charon perm-delete -i

Index | Created             | Original
--------------------------------------------------
1     | 2025-07-01 12:04:56 | /home/user/file
2     | 2025-06-27 15:18:02 | /home/user/directory
3     | 2025-07-02 12:28:27 | /other-file

Please enter a list of space-separated indices to delete.
For example: 1 3 5-12 15

>
```

### Empty

**Usage:**

```
Usage: charon empty [--prompt]

  Empties the trash.


Available options:
  --prompt                 Prompts before emptying the trash. This is the
                           default.

  --no-prompt              Disables --prompt.

  -h,--help                Show this help text
```

**Examples**

```
$ charon empty

Index | Created             | Original
--------------------------------------------------
1     | 2025-07-01 12:04:56 | /home/user/file
2     | 2025-06-27 15:18:02 | /home/user/directory
3     | 2025-07-02 12:28:27 | /other-file

Entries:      3
Total Files:  5
Log size:     1.38M
Size:         223.00K

Permanently delete all contents (y/n)?
```

## Restore Commands

### Restore

**Usage:**

```
Usage: charon restore [--force] [-i|--indices] [--prompt] [-v|--verbose]
                      [PATHS...]

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
  --force                  Forcibly overwrites restored path(s). Otherwise,
                           collisions with existing paths will either throw an
                           error (with --no-prompt) or prompt the user to
                           decide.

  --no-force               Disables --force.

  -i,--indices             Allows selecting by numeric index instead of trash
                           name. Incompatible with explicit paths. The prompt
                           can be exited via 'exit', 'quit', or ':q'.

  --no-indices             Disables --indices.

  --prompt                 Prompts before restoring path(s). This is the
                           default.

  --no-prompt              Disables --prompt.

  -v,--verbose             Lists restored paths.

  --no-verbose             Disables --verbose.

  -h,--help                Show this help text
```

**Examples**

```
# deleting "foo" and "baz" first
$ charon delete foo baz

# restore "foo" and "baz" to their original locations
$ charon restore foo baz

# restore via indices
$ charon restore -i

Index | Created             | Original
--------------------------------------------------
1     | 2025-07-01 12:04:56 | /home/user/file
2     | 2025-06-27 15:18:02 | /home/user/directory
3     | 2025-07-02 12:28:27 | /other-file

Please enter a list of space-separated indices to restore.
For example: 1 3 5-12 15

>
```

## Information Commands

### List

**Usage:**

```
Usage: charon list [--color ARG] [--format FMT] [-n|--name-len (max|NAT)]
                   [-o|--orig-len (max|NAT)] [-s|--sort (name|size)]
                   [-r|--reverse-sort]

  Lists all trash contents.


Available options:
  --color ARG              Coloring options.

                           - (t|true): On.
                           - (f|false): Off.
                           - (d|detect): On if supported.

  --format FMT             Formatting options.

                           - (m|multi): Prints each entry across multiple lines.
                           - (s|single): Compact, prints each entry across a
                           single lines.
                           - (t|tabular): The default. Prints a table that tries
                           to intelligently size the table based on available
                           terminal width and filename / original path lengths.
                           - (ts|tabular-simple): Simple table that does no
                           resizing. Prints the table with indices.

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
Usage: charon convert (-d|--dest (cbor|fdo|json))

  Converts the backend.


Available options:
  -d,--dest (cbor|fdo|json)
                           Backend to which we convert the current backend. See
                           --backend for more details.

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

# Installation

The [releases](https://github.com/tbidne/charon/releases) page has binaries built for several platforms. If there are no binaries for your platform, it is possible to [build charon](#building) yourself.

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 2.4+`](https://www.haskell.org/cabal/download.html)
* [`ghc 9.6 - 9.12`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

The current "blessed" version is `ghc-9.10.1`.

### Build Charon

Once you have `cabal` and `ghc`, `charon` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

> [!IMPORTANT]
>
> Charon requires git information to be available at build time, for the purposes of including some data in the binary (e.g. commit hash). Cabal's vanilla install method interfers with this, though we have a workaround that relies on passing the current directory as an environment variable:
>
> ```sh
> $ export CHARON_HOME=$(pwd); cabal install exe:charon
> ```
>
> Nix does not require such a workaround.

For further reproducibility, an optional freeze file can be used for the "blessed" compiler.

```sh
cabal build --project-file cabal.ghc<XYZ>.project
```

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
