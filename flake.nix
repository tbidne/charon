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
    inputs@{ algebra-simple
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
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            (pkgs.haskell.lib.dontCheck ghcid)
            (hlib.overrideCabal haskell-language-server (old: {
              configureFlags = (old.configureFlags or [ ]) ++
                [
                  "-f -brittany"
                  "-f -floskell"
                  "-f -fourmolu"
                  "-f -stylishhaskell"
                ];
            }))
          ];
          ghc-version = "ghc944";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "safe-rm";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if returnShellEnv then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                algebra-simple =
                  final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds = final.callCabal2nix "bounds" bounds { };
                byte-types =
                  final.callCabal2nix "byte-types" byte-types { };
                hedgehog = prev.hedgehog_1_2;
                effects-async =
                  final.callCabal2nix
                    "effects-async" "${monad-effects}/effects-async"
                    { };
                effects-exceptions =
                  final.callCabal2nix
                    "effects-exceptions" "${monad-effects}/effects-exceptions"
                    { };
                effects-fs =
                  final.callCabal2nix
                    "effects-fs" "${monad-effects}/effects-fs"
                    { };
                effects-ioref =
                  final.callCabal2nix
                    "effects-ioref" "${monad-effects}/effects-ioref"
                    { };
                effects-logger-ns =
                  final.callCabal2nix "effects-logger-ns"
                    "${monad-effects}/effects-logger-ns"
                    { };
                effects-optparse =
                  final.callCabal2nix "effects-optparse"
                    "${monad-effects}/effects-optparse"
                    { };
                effects-stm =
                  final.callCabal2nix "effects-stm"
                    "${monad-effects}/effects-stm"
                    { };
                effects-terminal =
                  final.callCabal2nix
                    "effects-terminal" "${monad-effects}/effects-terminal"
                    { };
                effects-thread =
                  final.callCabal2nix
                    "effects-thread" "${monad-effects}/effects-thread"
                    { };
                effects-time =
                  final.callCabal2nix
                    "effects-time" "${monad-effects}/effects-time"
                    { };
                package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
                path-size = final.callCabal2nix "path-size" path-size { };
                smart-math = final.callCabal2nix "smart-math" smart-math { };
                tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
              };
            };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
