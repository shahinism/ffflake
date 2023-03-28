{ pkgs, ... }:

{
  # Kernel
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_zen;

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  fonts.fonts = with pkgs; [
    roboto
    emacs-all-the-icons-fonts
    (nerdfonts.override { fonts = [ "FiraCode" "Hack" ]; })
  ];

  # Disable CUPS to print documents.
  services.printing.enable = false;

  # Enable pcscd, required for Yubikey to act like smart card
  services.pcscd.enable = true;
  # Enable ssh-agent
  programs.ssh.startAgent = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable Docker
  virtualisation.docker.enable = true;

  # Enable automatic login for the user.
  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
    # Configure keymap in X11
    layout = "us";
    xkbVariant = "";

    displayManager.lightdm = {
      enable = true;
      greeter.enable = true;
      # autoLogin.timeout = 0;
    };
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "shahin";
    displayManager.defaultSession = "none+qtile";

    # Enable the GNOME Desktop Environment.
    desktopManager.gnome.enable = true;
    # qtile
    windowManager.qtile.enable = true;

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

  };

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  # Automatic Nix garbage collection
  nix.gc.automatic = true;

  # Enable experimental features
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shahin = {
    shell = pkgs.bash;
    isNormalUser = true;
    description = "Shahin Azad";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [ nixFlakes ];
  };

  # dns servers
  networking.nameservers = [ "1.1.1.1" "1.0.0.1" ];
  networking.resolvconf.enable = pkgs.lib.mkForce false;
  networking.dhcpcd.extraConfig = "nohook resolv.conf";
  networking.networkmanager.dns = "none";
  services.resolved.enable = false;
  services.blueman.enable = true;

  # It's needed to explicitly disable this, as this is enabled by
  # default on Gnome, and prevents using tlp:
  # https://discourse.nixos.org/t/cant-enable-tlp-when-upgrading-to-21-05/13435
  services.power-profiles-daemon.enable = false;
  services.tlp = {
    enable = true;
    settings = {
      #https://linrunner.de/tlp/settings/platform.html
      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "low-power";

      # https://discourse.nixos.org/t/how-to-switch-cpu-governor-on-battery-power/8446/4
      # https://linrunner.de/tlp/settings/processor.html
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

      CPU_HWP_ON_AC = "balance_performance";
      CPU_HWP_ON_BAT = "power";

      CPU_MAX_PERF_ON_BAT = 30;

      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;

      # https://github.com/linrunner/TLP/issues/122
      # https://linrunner.de/tlp/settings/network.html
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "on";

      CPU_HWP_DYN_BOOST_ON_AC = 1;
      CPU_HWP_DYN_BOOST_ON_BAT = 0;

      SCHED_POWERSAVE_ON_AC = 0;
      SCHED_POWERSAVE_ON_BAT = 1;

      # https://linrunner.de/tlp/settings/runtimepm.html
      RUNTIME_PM_ON_AC = "on";
      RUNTIME_PM_ON_BAT = "auto";

      # https://linrunner.de/tlp/settings/usb.html
      USB_BLACKLIST_BTUSB = 1;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
