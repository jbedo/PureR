final: prev: {
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev:
      prev.haskell.packageOverrides hfinal hprev // {
        purer =
          let
            filesToIgnore = [
              "default.nix"
              "flake.nix"
              "flake.lock"
              ".git"
              ".github"
              "result"
              "shell.nix"
              ".stack-work"
              "stack.yaml"
              "stack.yaml.lock"
            ];

            src = builtins.path {
              # Naming this path makes sure that people will get the same
              # hash even if they checkout the purer repo into a
              # directory called something else.
              name = "purer-src";
              path = ../.;
              filter = path: type:
                with final.lib;
                ! elem (baseNameOf path) filesToIgnore &&
                ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
            };

          in
          hfinal.callCabal2nix "purer" src { };
      };
  };

  purer =
    final.haskell.lib.compose.justStaticExecutables final.haskellPackages.purer;

  hacking-on-purer-shell = final.haskellPackages.shellFor {
    withHoogle = false;
    packages = hpkgs: [ hpkgs.purer ];
    nativeBuildInputs = [
      final.cabal-install
      final.ghcid
      final.haskellPackages.haskell-language-server
      final.hlint
      final.purescript
      final.ormolu
      final.spago
    ];
  };

  use-purer-shell = final.stdenv.mkDerivation {
    name = "use-purer-shell";
    nativeBuildInputs = [
      final.purer
      final.purescript
      final.spago
    ];
    dontUnpack = true;
    installPhase = "touch $out";
  };
}
