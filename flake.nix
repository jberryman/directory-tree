{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        directoryTree = pkgs.haskellPackages.callCabal2nix "directory-tree" ./. {};
        overlay = (final: prev: {
          inherit directoryTree;
        });
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages = {
          directory-tree = directoryTree;
          default = directoryTree;
        };
        devShell = pkgs.haskellPackages.shellFor {
          name = "directory-tree";
          packages = p: [ directoryTree ];
          withHoogle = true;
          buildInputs = with pkgs; with pkgs.haskellPackages; [
            cabal-install
          ];
          shellHook = "command -v fish &> /dev/null && fish";
        };
      }
    );
}
