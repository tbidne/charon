.PHONY: build clean repl watch ;\
	cic ci formatc format lint lintc ;\
	haddock hackage

# dev

T = ""

build:
	if [ -z "$(T)" ]; then \
		cabal build; \
	else \
		cabal build $(T); \
	fi

clean:
	cabal clean

repl:
	if [ -z "$(T)" ]; then \
		cabal repl safe-rm; \
	else \
		cabal repl $(T); \
	fi

watch:
	if [ -z "$(T)" ]; then \
		ghcid --command "cabal repl safe-rm"; \
	else \
		ghcid --command "cabal repl $(T)"; \
	fi

watcht:
	find . -type f -name "*hs" | entr -s "cabal test $(T)"

bench:
	cabal bench --benchmark-options \
		'--csv benchmarks/bench.csv --svg benchmarks/bench.svg'

# ci

cic: formatc lintc

ci: lint format

# formatting

EXCLUDE_BUILD := ! -path "./.*" ! -path "./*dist-newstyle/*" ! -path "./*stack-work/*"
FIND_HS := find . -type f -name "*hs" $(EXCLUDE_BUILD)
FIND_CABAL := find . -type f -name "*.cabal" $(EXCLUDE_BUILD)

formatc:
	nixpkgs-fmt ./ --check && \
	$(FIND_CABAL) | xargs cabal-fmt --check && \
	$(FIND_HS) | xargs ormolu --mode check

format:
	nixpkgs-fmt ./ && \
	$(FIND_CABAL) | xargs cabal-fmt --inplace && \
	$(FIND_HS) | xargs ormolu -i

# linting

lint:
	$(FIND_HS) | xargs -I % sh -c " \
		hlint \
		--ignore-glob=dist-newstyle \
		--ignore-glob=stack-work \
		--refactor \
		--with-refactor=refactor \
		--refactor-options=-i \
		%"

lintc:
	hlint . --ignore-glob=dist-newstyle --ignore-glob=stack-work

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.8#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.8#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/safe-rm-0.1/opt/doc/html/safe-rm/* docs/

# generate dist and docs suitable for hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
