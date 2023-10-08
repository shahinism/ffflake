{ pkgs, config, ... }: {
  home.packages = with pkgs; [ atuin ];

  home.file.".local/share/atuin" = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/projects/personal/ffflake/home-manager/atuin";
  };
}
