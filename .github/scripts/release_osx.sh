#!/usr/bin/env bash

set -e

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

# x86_64 on macos-12/13, arm64 on macos-14
arch=$(uname -m)

# x86_64-osx on macos-12/13, aarch64-osx on macos-14
if [[ $arch == 'arm64' ]]; then
  # standardize name
  arch="aarch64"
fi

mkdir -p bin

suffix="$CHARON_VERS-$arch-macos_$apple_vers"

export CHARON_HOME=$(pwd); cabal install exe:charon --installdir bin/ --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/charon --help

echo "*** Printing version ***"
./bin/charon --version

echo "*** Computing sha256 ***"
sha256sum ./bin/charon > ./bin/charon.sha256
cat ./bin/charon.sha256

# -j needed to keep structure flat
zip "charon_$suffix.zip" -j ./bin/*
