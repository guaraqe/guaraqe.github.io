let
  commit = "a73246e2eef4c6ed172979932bc80e1404ba2d56";
  nixpkgs-src = builtins.fetchTarball {
    name = "nixos-unstable";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0dgfdbf25n29kwspazyzx81g8rj3nwrlfjij5yi8xkx6ylsd5bg3";
  };

in
  import nixpkgs-src
