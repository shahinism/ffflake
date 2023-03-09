# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./overlays.nix
    ];

  # System76
  hardware = {
    enableAllFirmware = true;
    cpu.intel.updateMicrocode = true;
    system76.enableAll = true;

    nvidia = {
      modesetting.enable = true;
      prime = {
        sync.enable = true;
        nvidiaBusId = "PCI:1:0:0";
        intelBusId = "PCI:0:2:0";
      };
    };

    opengl.driSupport32Bit = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  };

  

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Kernel
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_zen;
  
  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-f049a6fd-4311-49db-a162-e5ed47deda90".device = "/dev/disk/by-uuid/f049a6fd-4311-49db-a162-e5ed47deda90";
  boot.initrd.luks.devices."luks-f049a6fd-4311-49db-a162-e5ed47deda90".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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
    (nerdfonts.override { fonts = [ "FiraCode" "Hack" ]; })
  ];

  # Enable CUPS to print documents.
  services.printing.enable = true;
  
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
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };
  virtualisation.docker.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shahin = {
    shell = pkgs.zsh;
    isNormalUser = true;
    description = "Shahin Azad";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
      nixFlakes
    ];
  };

  # dns servers
  networking.nameservers = ["1.1.1.1" "1.0.0.1"];
  networking.resolvconf.enable = pkgs.lib.mkForce false;
  networking.dhcpcd.extraConfig = "nohook resolv.conf";
  networking.networkmanager.dns = "none";
  services.resolved.enable = false;
  services.blueman.enable = true; 

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

    videoDrivers = [ "nvidia" ];

    # Fix screen tearing
    screenSection = ''
  Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
  Option         "AllowIndirectGLXProtocol" "off"
  Option         "TripleBuffer" "on"
'';
  };

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  # Automatic Nix garbage collection
  nix.gc.automatic = true; 

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [];

  # Enable experimental features
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}
