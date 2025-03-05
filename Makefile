.PHONY: link-nixos
link-nixos:
	sudo mv /etc/nixos /etc/nixos.old
	sudo ln -s ~/_config /etc/nixos

.PHONY: link-macos
link-macos:
	sudo ln -s ~/_config /etc/nix-darwin
