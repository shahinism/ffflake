update:
	nix flake update
	nix flake lock

system76:
	sudo nixos-rebuild switch --flake .#system76

framework:
	sudo nixos-rebuild switch --flake .#framework
