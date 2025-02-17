{
  description = "My emacs exploration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = {nixpkgs, ...}: let
    pkgs = nixpkgs.legacyPackages."x86_64-linux";
  in {
    devShells."x86_64-linux".default = pkgs.mkShell {
      buildInputs = with pkgs; [
        emacs
      ];
    };
  };
}
