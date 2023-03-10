{pkgs, config, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };
  
  services.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

 home.file = {
   ".emacs.d" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/projects/personal/ffflake/home-manager/emacs/";
    };
  };
}
