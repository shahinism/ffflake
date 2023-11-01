{ pkgs, ... }: {
  programs.zellij = {
    enable = true;
    package = pkgs.zellij;
  };

  home.file.".config/zellij".source = ./zellij;
}
