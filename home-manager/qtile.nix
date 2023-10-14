{ config, ... }: {
  home.file = {
    ".config/qtile" = {
      source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/.config/ffflake/home-manager/qtile/";
    };
  };
}
