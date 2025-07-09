#!/usr/bin/env bash

set -e

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="$CHARON_VERS-$arch-linux"

docker build \
  -t charon_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg CABAL_VERS=$CABAL_VERS \
  --build-arg CABAL_PROJ=$CABAL_PROJ \
  --build-arg GHC_VERS=$GHC_VERS \
  .

cp docker_out/charon bin/

echo "*** Testing exe ***"
./bin/charon --help

echo "*** Printing version ***"
./bin/charon --version

echo "*** Computing sha256 ***"
sha256sum ./bin/charon > ./bin/charon.sha256
cat ./bin/charon.sha256

# -j needed to keep structure flat
zip "charon_$suffix.zip" -j ./bin/*
