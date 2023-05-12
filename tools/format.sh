set -e

export LANG="C.UTF-8"

export hs_dirs="app benchmarks lib src test"

nixpkgs-fmt ./

cabal-fmt --inplace safe-rm.cabal

# shellcheck disable=SC2046,SC2086
ormolu -m inplace $(find $hs_dirs -type f -name '*.hs')