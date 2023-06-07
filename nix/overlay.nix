_: pkgs:

let
  overrides = _: hspkgs: with pkgs.haskell.lib;
    {
    };
in

{
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions
        (old.overrides or (_: _: {}))
        overrides;
  });
}
