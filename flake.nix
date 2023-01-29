{
  description = "A simple utility for deleting files";
  inputs = {
    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    byte-types = {
      url = "github:tbidne/byte-types";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects/";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.smart-math.follows = "smart-math";
    };
    path-size = {
      url = "github:tbidne/path-size/";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.byte-types.follows = "byte-types";
      inputs.monad-effects.follows = "monad-effects";
      inputs.smart-math.follows = "smart-math";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    { algebra-simple
    , bounds
    , byte-types
    , flake-compat
    , flake-parts
    , monad-effects
    , nixpkgs
    , path-size
    , self
    , smart-math
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc925";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv: withDevTools:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "safe-rm";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if withDevTools then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                algebra-simple =
                  final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds = final.callCabal2nix "bounds" bounds { };
                byte-types =
                  final.callCabal2nix "byte-types" byte-types { };
                hedgehog = prev.hedgehog_1_2;
                monad-async =
                  final.callCabal2nix
                    "monad-async" "${monad-effects}/monad-async"
                    { };
                monad-exceptions =
                  final.callCabal2nix
                    "monad-exceptions" "${monad-effects}/monad-exceptions"
                    { };
                monad-fs =
                  final.callCabal2nix
                    "monad-fs" "${monad-effects}/monad-fs"
                    { };
                monad-ioref =
                  final.callCabal2nix
                    "monad-ioref" "${monad-effects}/monad-ioref"
                    { };
                monad-logger-namespace =
                  final.callCabal2nix "monad-logger-namespace"
                    "${monad-effects}/monad-logger-namespace"
                    { };
                monad-optparse =
                  final.callCabal2nix "monad-optparse"
                    "${monad-effects}/monad-optparse"
                    { };
                monad-stm =
                  final.callCabal2nix "monad-stm"
                    "${monad-effects}/monad-stm"
                    { };
                monad-terminal =
                  final.callCabal2nix
                    "monad-terminal" "${monad-effects}/monad-terminal"
                    { };
                monad-thread =
                  final.callCabal2nix
                    "monad-thread" "${monad-effects}/monad-thread"
                    { };
                monad-system-time =
                  final.callCabal2nix
                    "monad-system-time" "${monad-effects}/monad-system-time"
                    { };
                package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
                path-size = final.callCabal2nix "path-size" path-size { };
                smart-math = final.callCabal2nix "smart-math" smart-math { };
                tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;

                # Until we can upgrade nixpkgs. Current problems:
                # - flake-parts warning on deps
                # - ghcid broken thus deps not upgraded yet
                toml-reader = final.callHackageDirect
                  {
                    pkg = "toml-reader";
                    ver = "0.2.0.0";
                    sha256 = "sha256-/A40+3QFwWq0Q6KOfB+9tgEiQBWWgW2zutpaz4RmjeQ=";
                  }
                  { };
              };
            };
        in
        {
          packages.default = mkPkg false false;
          devShells.default = mkPkg true true;
          devShells.ci = mkPkg true false;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
