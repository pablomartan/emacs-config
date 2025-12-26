{
  description = "My emacs exploration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nix-emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    nix-emacs,
    ...
  }: let
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [
        nix-emacs.overlays.default
      ];
    };
  in {
    packages."x86_64-linux".default = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs.override {withGTK3 = true;};
      config = ./init.el;
      alwaysEnsure = true;
      defaultInitFile = true;
    };
  };
}
