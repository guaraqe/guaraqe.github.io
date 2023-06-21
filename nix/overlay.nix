_: pkgs:

let

  latex-formulae-src = pkgs.fetchFromGitHub {
    owner = "guaraqe";
    repo = "latex-formulae";
    rev = "31ea096c55a34f53cbeeadb30a6514e19f4d1c95";
    sha256 = "sha256-paOJ8q9aJttjwumOA8y47c6SRqvKv+Rj79mgv6TJaZ0=";
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
