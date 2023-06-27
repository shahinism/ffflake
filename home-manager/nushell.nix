{ pkgs, config, ... }: {
  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  home.file.".config/nushell" = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/projects/personal/ffflake/home-manager/nushell/";
  };
}
