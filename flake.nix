{
  description = "guaraqe.com website";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/2d293cbfa5a793b4c50d17c05ef9e385b90edf6c";

  outputs = { nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          haskellPackages = pkgs.haskell.packages.ghc9103;
        in
        {
          default = pkgs.mkShell {
            packages = [
              # A bare GHC rather than ghcWithPackages: the builder is an
              # ordinary cabal project, so cabal resolves and builds its
              # dependencies itself. Listing them here as well only made the
              # two sets drift apart.
              haskellPackages.ghc
              haskellPackages.cabal-install
              pkgs.ghcid
              pkgs.ormolu
              # Not in the ghc9103 set at a version that builds; the formatter
              # is a standalone tool, so the default set is fine.
              pkgs.haskellPackages.cabal-fmt

              # `make serve` runs browser-sync over docs/.
              pkgs.nodePackages.browser-sync
              pkgs.nodejs

              # The builder shells out to this to compile site/css/tailwind.css.
              # Pinned to v3 to match the config and the typography plugin.
              pkgs.tailwindcss_3

              # Shake's forward mode traces external commands through fsatrace
              # to discover their dependencies.
              pkgs.fsatrace

              # The PDF CV is authored in Typst; poppler and ImageMagick render
              # and inspect the result.
              pkgs.typst
              pkgs.poppler-utils
              pkgs.imagemagick

              pkgs.translate-shell

              # Cabal builds some dependencies against C libraries and locates
              # them with pkg-config.
              pkgs.pkg-config
            ];

            # In buildInputs rather than packages so the setup hook exposes the
            # dev output: cabal needs the headers and the .pc file, not just
            # the shared object.
            buildInputs = [
              pkgs.zlib
            ];
          };
        });
    };
}
