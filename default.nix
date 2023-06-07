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
        withHoogle = true;
        buildInputs =
          [
            nodePackages.npm
            nodePackages.node2nix
            # Haskell
            localHaskellPackages.ghc
            localHaskellPackages.cabal-install
            localHaskellPackages.ghcid
            localHaskellPackages.ormolu
            localHaskellPackages.cabal-fmt
          ];
      };
in
  if forShell
  then shell
  else packages
