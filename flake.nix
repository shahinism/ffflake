{
  description = "My Personal Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    emacs-src.url = "github:emacs-mirror/emacs/emacs-29";
    emacs-src.flake = false;
    
    home-manager = {                                                      # User Package Management
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
    
  outputs = { self, nixpkgs, nixos-hardware, home-manager, emacs-overlay, emacs-src }: 
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        # overlays = [ (import self.inputs.emacs-overlay) ];
        overlays = [
          emacs-overlay.overlays.default
          (final: prev: {
            emacs29 = (prev.emacsGit.override {
              
            }).overrideAttrs(old: {
              name = "emacs29";
              version = "29.0-${emacs-src.shortRev}";
              src = emacs-src;
            });
          })
        ];
      };
      lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        system76 = lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./overlays.nix
            ./shared.nix
            ./system76/configuration.nix
            nixos-hardware.nixosModules.system76
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.shahin = { ... }: {
                  imports = [
                    ./home-manager/home.nix
                  ];
                };
              };
            }
          ];
        };

        framework = lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./overlays.nix
            ./shared.nix
            ./framework/configuration.nix
            nixos-hardware.nixosModules.framework-12th-gen-intel
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.shahin = { ... }: {
                  imports = [
                    ./home-manager/home.nix
                  ];
                };
              };
            }
          ];
        };
      };
    };
}
