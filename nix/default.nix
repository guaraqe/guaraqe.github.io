import ./nixpkgs.nix {
  overlays = [ (import ./overlay.nix) ];
  config = { allowBroken = true; };
}
