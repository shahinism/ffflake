{ config, pkgs, devenv, ... }:

let
  python-packages = p:
    with p;
    [
      # python-lsp-server
      ipython
    ];
  nodejs-packages = with pkgs.nodePackages_latest;
    [
      # vscode-langservers-extracted
      # typescript-language-server
      js-beautify # used with Emacs' web-beautify
    ];
  devpkgs = devenv.packages.x86_64-linux;
in {
  imports = [
    ./firefox.nix
    ./nushell.nix
    ./bash.nix
    ./aliases.nix
    ./git.nix
    ./dunst.nix
    ./emacs.nix
    ./qtile.nix
    ./rofi.nix
    ./tmux.nix
  ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "shahin";
  home.homeDirectory = "/home/shahin";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Homse Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs;
    [
      gnumake
      cmake # rquired by emacs to build vterm
      gcc
      git-extras
      tmux # TODO tune me
      silver-searcher
      okular

      nodejs # required by copilot
      aspell # Used with Emacs as spell checker
      aspellDicts.en
      aspellDicts.en-science
      aspellDicts.en-computers

      # Required for Emacs vterm
      libvterm
      libtool

      unzip # crucial for company-tabnine to unzip the package
      # otherwise you'll have an empty directory

      killall
      xorg.xkill

      htop
      brave
      slack
      xclip
      fzf
      gnupg
      ripgrep

      kitty

      pinentry-gtk2

      ranger
      direnv
      rpi-imager
      flameshot
      keybase-gui
      networkmanagerapplet
      blueman
      xss-lock
      udiskie
      betterlockscreen
      brightnessctl

      pulseaudioFull
      obs-studio
      vlc
      gnome.gnome-tweaks

      yubikey-personalization
      yubikey-manager
      pcscliteWithPolkit

      autorandr

      aws-vault
      awscli2
      ssm-session-manager-plugin

      libreoffice

      hugo

      # Nix tools
      comma # Run binaries without installing them!
      cachix # Service for Nix binary cache hosting
      nixos-option # Inspect NixOS configuration

      # Python
      (python3.withPackages python-packages)

      bash-completion
      # zsh-completions
      docker-compose
      # Modern CLI tools
      # https://zaiste.net/posts/shell-commands-rust/
      # dust
      procs
      bottom
      tealdeer
      bandwhich
      zoxide
      fd
      rm-improved

      # AppImage
      appimage-run
      jq
      asciinema

      anki-bin
      exercism

      # Common Lisp
      sbcl
      rlwrap

      mosh
      magic-wormhole
      sshuttle
    ] ++ nodejs-packages ++ [ devpkgs.devenv ];

  programs = {
    starship.enable = true;
    exa.enable = true; # the ls replacement

    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [ batdiff batman batwatch ];
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };

  # Enable keybase requirements
  services.kbfs.enable = true;
  services.keybase.enable = true;

  # TODO declarative configuration with secrets
  services.syncthing = { enable = true; };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "application/x-extension-htm" = "firefox.desktop";
      "application/x-extension-html" = "firefox.desktop";
      "application/x-extension-shtml" = "firefox.desktop";
      "application/x-extension-xhtml" = "firefox.desktop";
      "application/x-extension-xht" = "firefox.desktop";
      "audio/x-ms-asx" = "vlc.desktop";
      "audio/x-ms-wma" = "vlc.desktop";
      "audio/mp2" = "vlc.desktop";
      "audio/x-mpegurl" = "vlc.desktop";
      "audio/ogg" = "vlc.desktop";
      "audio/x-scpls" = "vlc.desktop";
      "audio/mpeg" = "vlc.desktop";
      "audio/x-wav" = "vlc.desktop";
      "audio/aac" = "vlc.desktop";
      "audio/mp4" = "vlc.desktop";
      "audio/vnd.rn-realaudio" = "vlc.desktop";
      "text/x-c++src" = "emacsclient.desktop";
      "text/x-pascal" = "emacsclient.desktop";
      "text/x-google-video-pointer" = "emacsclient.desktop";
      "text/x-c++hdr" = "emacsclient.desktop";
      "text/html" = "emacsclient.desktop";
      "text/plain" = "emacsclient.desktop";
      "text/tcl" = "emacsclient.desktop";
      "text/x-csrc" = "emacsclient.desktop";
      "text/x-makefile" = "emacsclient.desktop";
      "text/x-chdr" = "emacsclient.desktop";
      "text/x-tex" = "emacsclient.desktop";
      "text/x-java" = "emacsclient.desktop";
      "text/x-moc" = "emacsclient.desktop";
      "video/3gpp" = "vlc.desktop";
      "video/ogg" = "vlc.desktop";
      "video/quicktime" = "vlc.desktop";
      "video/x-ms-wmv" = "vlc.desktop";
      "video/vnd.rn-realvideo" = "vlc.desktop";
      "video/mp4" = "vlc.desktop";
      "video/mpeg" = "vlc.desktop";
      "video/x-matroska" = "vlc.desktop";
      "video/x-flic" = "vlc.desktop";
      "video/x-msvideo" = "vlc.desktop";
      "video/x-theora+ogg" = "vlc.desktop";
      "video/x-flv" = "vlc.desktop";
      "application/x-xpinstall" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "application/xml" = "firefox.desktop";
      "application/json" = "firefox.desktop";
    };
  };
}
