{ pkgs, ... }: {
  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };
}
