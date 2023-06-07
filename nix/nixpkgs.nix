let
  commit = "0591d6b57bfeb55dfeec99a671843337bc2c3323";
  nixpkgs-src = builtins.fetchTarball {
    name = "nixos-unstable";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "sha256-TbQeQcM5TA/wIho6xtzG+inUfiGzUXi8ewwttiQWYJE=";
  };

in
  import nixpkgs-src
