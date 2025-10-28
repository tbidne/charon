{
  description = "A simple utility for deleting files";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.smart-math.follows = "smart-math";
    };
    path-size = {
      url = "github:tbidne/path-size";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.si-bytes.follows = "si-bytes";
      inputs.monad-effects.follows = "monad-effects";
      inputs.smart-math.follows = "smart-math";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    unicode-grapheme = {
      url = "github:tbidne/unicode-grapheme";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      monad-effects,
      nix-hs-utils,
      nixpkgs,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc9122";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                # These are flaky.
                auto-update = hlib.dontCheck prev.auto-update;
                unicode-data = hlib.dontCheck prev.unicode-data;

                Cabal-syntax_3_10_3_0 = hlib.doJailbreak prev.Cabal-syntax_3_10_3_0;
                haskeline = prev.haskeline_0_8_4_0;

                # TODO: Remove optparse override and jailbreaks once former
                # is the default.
                fourmolu = hlib.doJailbreak prev.fourmolu;
                hspec-golden = hlib.doJailbreak prev.hspec-golden;
                ormolu = hlib.doJailbreak prev.ormolu;
                optparse-applicative = prev.optparse-applicative_0_19_0_0;
                stylish-haskell = hlib.doJailbreak prev.stylish-haskell;
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "path-size"
                "si-bytes"
                "smart-math"
              ]
              // nix-hs-utils.mkRelLibs "${monad-effects}/lib" final [
                "effects-async"
                "effects-ioref"
                "effects-fs"
                "effects-haskeline"
                "effects-logger"
                "effects-optparse"
                "effects-stm"
                "effects-terminal"
                "effects-thread"
                "effects-time"
                "effects-unix"
                "effects-unix-compat"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.unicode-grapheme}/lib" final [
                "unicode-grapheme"
              ];
          };
          hlib = pkgs.haskell.lib;
          compilerPkgs = { inherit compiler pkgs; };
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "charon";
              root = ./.;

              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  CHARON_HASH = "${self.rev or self.dirtyRev}";
                  CHARON_MODIFIED = "${builtins.toString self.lastModified}";
                  CHARON_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";

                  # Git is needed to run the tests (git diff).
                  nativeBuildInputs = oldAttrs.nativeBuildInputs or [ ] ++ [
                    pkgs.git
                  ];
                });
            };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            lint = nix-hs-utils.lint compilerPkgs;
            lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
