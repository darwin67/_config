# Config files and dotfiles

Settings that are shared across computers.

## NixOS setup

```sh
# back it up
sudo mv /etc/nixos /etc/nixos.old

sudo ln -s ~/_config/nix /etc/nixos
```

