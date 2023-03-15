{ config, pkgs, ... }:

let
  python-packages = p: with p; [
    # python-lsp-server
    ipython
  ];
  nodejs-packages = with pkgs.nodePackages_latest; [
    # vscode-langservers-extracted
    # typescript-language-server
    js-beautify # used with Emacs' web-beautify
  ];
in {
  imports = [
    ./firefox.nix
    ./bash.nix
    ./aliases.nix
    ./git.nix
    ./dunst.nix
    ./emacs.nix
    ./qtile.nix
    ./rofi.nix
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
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    gnumake
    cmake     # rquired by emacs to build vterm
    gcc
    git-extras
    tmux      # TODO tune me
    silver-searcher

    # Required for Emacs vterm
    libvterm
    libtool

    unzip   # crucial for company-tabnine to unzip the package
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
    awscli
    ssm-session-manager-plugin

    libreoffice

    hugo

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
  ] ++ nodejs-packages;

  programs = {
    starship.enable = true;
    exa.enable = true;   # the ls replacement
    
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
  services.syncthing = {
    enable = true;
  };  
}
