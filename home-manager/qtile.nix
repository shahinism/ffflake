{config, ...}:
{
  home.file = {
    ".config/qtile" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/projects/personal/ffflake/home-manager/qtile/";
    };
  };
}
