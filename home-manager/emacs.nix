{pkgs, ...}:

{
  shahin-emacs29 = ((pkgs.emacsPackagesFor pkgs.emacs29).emacsWithPackages(epkgs: with epkgs; [ vterm ]));
  services.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

  home.file = {
    ".emacs.d" = {
      source = ./. + "/emacs";
    };
  };
}
