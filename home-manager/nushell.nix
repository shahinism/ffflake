{ pkgs, config, ... }: {
  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  # https://github.com/rsteube/carapace-bin
  home.packages = with pkgs; [ carapace ];

  home.file.".config/nushell" = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/projects/personal/ffflake/home-manager/nushell/";
  };
}
