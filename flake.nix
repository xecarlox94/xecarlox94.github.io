{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:

    flake-utils.lib.eachDefaultSystem (system:
      let

        pkgs = import nixpkgs { inherit system; };

      in
      {
        packages.default = pkgs.cowsay;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            zola
          ];
        };
      }
    );
}
