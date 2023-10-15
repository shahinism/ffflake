# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  hardware = {
    enableAllFirmware = true;
    cpu.intel.updateMicrocode = true;
  };
  services.fwupd.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Solve 12th Gen (Alder Lake) with X server
  # https://nixos.wiki/wiki/Intel_Graphics
  boot.kernelParams = [ "i915.force_probe=46a6" ];

  nixpkgs.config.packageOverrides = pkgs: {
    vaapIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel # LIBVA_DRIVER_NAME=1965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  # Setup keyfile
  boot.initrd.secrets = { "/crypto_keyfile.bin" = null; };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-94d8bf3c-91b1-43d3-9d8e-6420c460881d".device =
    "/dev/disk/by-uuid/94d8bf3c-91b1-43d3-9d8e-6420c460881d";
  boot.initrd.luks.devices."luks-94d8bf3c-91b1-43d3-9d8e-6420c460881d".keyFile =
    "/crypto_keyfile.bin";

  networking.hostName = "framework"; # Define your hostname.

  services.xserver.videoDrivers = [ "modesetting" ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
