{ pkgs, config, ... }:

let
  emacsClientGuiDesktop = pkgs.makeDesktopItem {
    desktopName = "Emacs Client GUI";
    name = "Emacs Client GUI";
    genericName = "Text Editor";
    comment = "Edit Text";
    mimeTypes = [
      "text/english"
      "text/plain"
      "text/x-makefile"
      "text/x-c++hdr"
      "text/x-c++src"
      "text/x-chdr"
      "text/x-csrc"
      "text/x-java"
      "text/x-moc"
      "text/x-pascal"
      "text/x-tcl"
      "text/x-tex"
      "application/x-shellscript"
      "text/x-c"
      "text/x-c++"
    ];
    exec = ''emacsclient -c -a "emacs" %F'';
    icon = "emacs";
    terminal = false;
    categories = [ "Development" "TextEditor" "Utility" ];
    startupWMClass = "Emacs";
  };
in {
  home.packages = [ emacsClientGuiDesktop ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    defaultEditor = true;
  };

  home.file = {
    ".emacs.d" = {
      source = config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/.config/ffflake/home-manager/emacs.chemacs2/";
    };
  };

  home.file = {
    ".emacs-profiles.el" = {
      source = ./emacs-profile.el;
    };
  };
}
