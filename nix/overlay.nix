_: pkgs:

let

  #latex-formulae-src = ../tmp/latex-formulae;

  latex-formulae-src = pkgs.fetchFromGitHub {
    owner = "guaraqe";
    repo = "latex-formulae";
    rev = "6b5ec380957c924f31bc4fb1e73dfc0f476f59b1";
    sha256 = "sha256-ztQBE0ONKk2M+5Vvt7ZEr4nShpf0v4lcAOJ9sXn7ba4=";
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
