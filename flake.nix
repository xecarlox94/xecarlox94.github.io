{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { nixpkgs, ... }: {

    devShells.x86_64-linux.default =
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
      in
      with pkgs;
        mkShell {

          packages = [
            cabal-install
            zlib
          ] ++
          ( with haskellPackages; [
              hakyll
            ]
          );

          shellHook = ''
            export DEBUG=1
          '';

        };

  };
}
