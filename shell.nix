{ pkgs ? import <nixpkgs> {} }:

let
  python-packages = p: with p; [
    pip
    python-lsp-server
    black
  ];
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (python3.withPackages python-packages)
  ];
}
