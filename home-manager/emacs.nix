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
    extraPackages = (epkgs: (with epkgs.melpaPackages; [
      anaconda-mode
      blacken
      cape
      cargo-mode
      chatgpt-shell
      consult
      consult-ag
      consult-eglot
      # copilot
      corfu
      # corfu-terminal
      crux
      devdocs
      direnv
      docker
      docker-compose-mode
      dockerfile-mode
      # eat
      editorconfig
      # eglot
      embark
      embark-consult
      exec-path-from-shell
      exercism
      go-mode
      hydra
      indent-guide
      magit
      major-mode-hydra
      marginalia
      markdown-mode
      markdown-toc
      meow
      nix-mode
      nushell-mode
      orderless
      org-bullets
      org-download
      org-roam
      # rainbow-mode
      rust-mode
      shell-maker
      # sql
      typescript-mode
      undo-fu-session
      vertico
      # vundo
      web-mode
      yaml-mode
      zoxide
    ]));
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
