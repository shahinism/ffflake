{ pkgs, ... }:

let python-packages = p: with p; [ pip python-lsp-server ];
in {
  # https://devenv.sh/basics/
  env.GREET = "Shahinism";

  # # https://devenv.sh/packages/
  packages = with pkgs; [ (python3.withPackages python-packages) ];

  # # https://devenv.sh/scripts/
  # scripts.hello.exec = "echo hello from $GREET";

  # enterShell = ''
  #   hello
  #   git --version
  # '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;
  languages.python.enable = true;

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks = {
    shellcheck.enable = true;
    black.enable = true;
    nixfmt.enable = true;
  };
}
