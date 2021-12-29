{
  description = "PureR";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = import nix/overlay.nix;
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
        in
        {
          defaultPackage = pkgs.purer;
          packages.purer = pkgs.purer;

          devShell = pkgs.hacking-on-purer-shell;
          devShells = {
            # This shell is for hacking on purer itself.  You get GHC with a
            # suitable package database, as well as a bunch of common Haskell
            # development tools.  You also get purs and spago that can be used for
            # testing out purer.
            hacking-on-purer = pkgs.hacking-on-purer-shell;
            # This is a shell that contains purer, purs, and spago.  It will
            # mainly be used by the flake.nix for all our PureScript packages.
            # It can also be used by users who just want to play around with
            # purer, but not hack on it.
            use-purer = pkgs.use-purer-shell;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
