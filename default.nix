{ forShell ? false }:

with import ./nix;
with builtins;

let
  targets = {
    builder = ./builder;
  };

  # Add local packages to haskellPackages.
  localHaskellPackages =
    haskellPackages.extend (haskell.lib.packageSourceOverrides targets);

  # Set of local packages, built from targets.
  packages =
    mapAttrs (name: _: localHaskellPackages.${name}) targets;

  # Shell for developing the local packages.
  shell =
    localHaskellPackages.shellFor
      {
        packages = _: attrValues packages;
        buildInputs =
          [
            # Site
            nodePackages.browser-sync
            nodePackages.npm
            nodePackages.node2nix
            # Haskell
            localHaskellPackages.ghc
            localHaskellPackages.cabal-install
            localHaskellPackages.ghcid
            localHaskellPackages.ormolu
            localHaskellPackages.cabal-fmt
            # Latex
            texlive.combined.scheme-medium
            imagemagick
            # Translate
            translate-shell
          ];
      };
in
  if forShell
  then shell
  else packages
