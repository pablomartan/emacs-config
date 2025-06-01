{
  description = "My emacs exploration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nix-emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    nix-emacs,
    self,
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
      extraEmacsPackages = epkgs:
        with epkgs; [
          (treesit-grammars.with-grammars (g:
            with g; [
              tree-sitter-bash
              tree-sitter-bibtex
              tree-sitter-css
              tree-sitter-dockerfile
              tree-sitter-elisp
              tree-sitter-html
              tree-sitter-json
              tree-sitter-latex
              tree-sitter-ledger
              tree-sitter-lua
              tree-sitter-markdown
              tree-sitter-nix
              tree-sitter-python
              tree-sitter-sql
              tree-sitter-tsx
              tree-sitter-typescript
            ]))
        ];
    };
    devShells."x86_64-linux".default = pkgs.mkShell {
      buildInputs = [
        self.packages."x86_64-linux".default
      ];
    };
  };
}
