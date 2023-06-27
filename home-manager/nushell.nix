{ pkgs, ... }: {
  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };
  programs.atuin.enable = true;

  home.file.".config/nushell".source = ./nushell;
}
