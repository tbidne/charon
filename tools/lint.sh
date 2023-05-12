set -e

export LANG="C.UTF-8"

export dirs="app benchmarks lib src test"

# shellcheck disable=SC2046,SC2086
hlint $(find $dirs -type f -name "*.hs")