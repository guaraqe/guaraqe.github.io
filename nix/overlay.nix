_: pkgs:

let

  #latex-formulae-src = ../tmp/latex-formulae;

  latex-formulae-src = pkgs.fetchFromGitHub {
    owner = "guaraqe";
    repo = "latex-formulae";
    rev = "8776ea60a47f2ef88376975c0b85c0a149fc6785";
    sha256 = "sha256-HvLdQIy2ZhQiZH/YxMptsKeRHpoOY29g56Y77upODeE=";
  };

  overrides = _: hspkgs: with pkgs.haskell.lib;
    { latex-formulae-image =
        hspkgs.callCabal2nix "latex-formulae-image" "${latex-formulae-src}/latex-formulae-image" {};

      latex-formulae-pandoc =
        hspkgs.callCabal2nix "latex-formulae-pandoc" "${latex-formulae-src}/latex-formulae-pandoc" {};
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
