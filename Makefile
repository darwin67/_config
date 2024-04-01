.PHONY: update-nixos
update-nixos:
	sudo cp ./nix/nixos.nix /etc/nixos/configuration.nix
	sudo nixos-rebuild test
