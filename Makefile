update:
	nix flake update
	nix flake lock

system76:
	sudo nixos-rebuild switch --flake .#system76

system76-offline:
	sudo nixos-rebuild switch --option binary-caches "" --flake .#system76

framework:
	sudo nixos-rebuild switch --flake .#framework

framework-offline:
	sudo nixos-rebuild switch --option binary-caches "" --flake .#framework
