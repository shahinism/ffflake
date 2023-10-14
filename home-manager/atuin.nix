{ pkgs, config, ... }: {
  home.packages = with pkgs; [ atuin ];

  home.file.".local/share/atuin" = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.config/ffflake/home-manager/atuin";
  };
}
