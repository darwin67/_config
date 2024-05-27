.PHONY: update-nixos
update-nixos:
	sudo cp ./nix/nixos.d/luks.nix /etc/nixos/luks.nix
	sudo cp ./nix/nixos.nix /etc/nixos/configuration.nix
