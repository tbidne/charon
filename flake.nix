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
    # TODO: [Haskeline override]
    #
    # Remove once new release in nixpkgs.
    haskeline = {
      url = "github:haskell/haskeline";
      flake = false;
    };
    monad-effects = {
      url = "github:tbidne/monad-effects/haskeline";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";

      # see TODO: [Haskeline override]
      inputs.haskeline.follows = "haskeline";

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
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                # These are flaky.
                auto-update = hlib.dontCheck prev.auto-update;

                # see TODO: [Haskeline override]
                haskeline = hlib.dontCheck (final.callCabal2nix "haskeline" inputs.haskeline { });

                path = hlib.dontCheck prev.path_0_9_6;
                serialise = hlib.doJailbreak prev.serialise;

                gitrev-typed = (
                  final.callHackageDirect {
                    pkg = "gitrev-typed";
                    ver = "0.1";
                    sha256 = "sha256-s7LEekR7NLe3CNhD/8uChnh50eGfaArrrtc5hoCtJ1A=";
                  } { }
                );
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

              # TODO: Once hlint is back to working with our GHC we can
              # use nix-hs-utils.mkDevTools ++ webDeps.
              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
            };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            #lint = nix-hs-utils.lint compilerPkgs;
            #lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
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
