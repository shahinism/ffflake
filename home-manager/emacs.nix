{pkgs, ...}:

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
      source = ./. + "/emacs";
      recursive = true;
    };
  };
}
