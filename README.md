# Config files and dotfiles

Settings that are shared across computers.

## Dynamic Wallpapers

```
./scripts/dynamic-wallpapers

cd ~/Pictures/DynamicWallpapers
ls | xargs -I{} sudo heic-install {}
```

## Add user to groups for access to tools

```
sudo usermod -aG docker $USER
sudo usermod -aG nix-users $USER
```
