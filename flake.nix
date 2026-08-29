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
          ghc = haskellPackages.ghcWithPackages (packages: with packages; [
            aeson
            blaze-html
            blaze-markup
            bytestring
            containers
            directory
            hashable
            http-client
            http-client-tls
            lens
            lens-aeson
            pandoc
            pandoc-crossref
            pandoc-types
            shake
            slick
            text
            time
            unordered-containers
          ]);
        in
        {
          default = pkgs.mkShell {
            packages = [
              ghc
              haskellPackages.cabal-install
              # The builder shells out to this to compile site/css/tailwind.css.
              # Pinned to v3 to match the config and the typography plugin.
              pkgs.tailwindcss_3
              # Shake's forward mode traces external commands through fsatrace
              # to discover their dependencies.
              pkgs.fsatrace
            ];
          };
        });
    };
}
