# Config files and dotfiles

Settings that are shared across computers.

## NixOS setup

```sh
# back it up
sudo mv /etc/nixos /etc/nixos.old

sudo ln -s ~/_config /etc/nixos
```

## macOS setup

```sh
sudo ln -s ~/_config /etc/nix-darwin

darwin-rebuild switch
```
