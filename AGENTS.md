# AGENTS.md

## Repository Overview

This repository manages reproducible workstation configuration for Linux with
NixOS and macOS with nix-darwin. It includes system configuration, Home Manager
modules, dotfiles, editors, shells, terminals, and desktop environments.

## Repository Layout

- `flake.nix`: Flake outputs, system configurations, and development shells.
- `nixos/`: Shared, platform-specific, and host-specific system modules.
- `dots/`: General dotfiles.
- `editor/`: Editor configuration.
- `sway/`: Sway and Wayland configuration.
- `term/`: Terminal emulator configuration.
- `zsh/`: Shell configuration and functions.

Prefer shared modules for behavior used by multiple hosts. Keep operating-system
and host-specific settings in their existing platform or host directories.

## Common Commands

```sh
# Enter the development environment
nix develop

# Validate flake outputs and configurations
nix flake check

# Apply a NixOS configuration
sudo nixos-rebuild switch

# Apply a macOS configuration
darwin-rebuild switch

# Link this repository as the system configuration
make link-nixos
make link-macos
```

## Change Guidelines

- Follow the style and structure of nearby modules.
- Keep changes scoped to the relevant platform, host, or application.
- Reuse existing shared modules instead of duplicating configuration.
- Format changed Nix files with the repository's available Nix formatter.
- Run `nix flake check` for changes that affect flake or system evaluation.
- Do not commit unencrypted secrets. Secrets are managed with sops-nix.
