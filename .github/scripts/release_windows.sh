#!/usr/bin/env bash

set -e

windows_vers=$1

arch="x86_64"

mkdir -p bin

suffix="$CHARON_VERS-$arch-windows_$windows_vers"

export CHARON_HOME=$(pwd); cabal install exe:charon --installdir bin/ --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/charon --help

echo "*** Printing version ***"
./bin/charon --version

echo "*** Computing sha256 ***"
sha256sum ./bin/charon > ./bin/charon.sha256
cat ./bin/charon.sha256

7z a "charon_$suffix.zip" ./bin/*
