# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is Darwin's personal workstation configuration repository using Nix/NixOS for reproducible system management. It contains dotfiles, system configurations, and development environment setups for both Linux (NixOS) and macOS (nix-darwin) systems.

## Core Architecture

### Flake Structure
- **flake.nix**: Main entry point defining system configurations and development shells
- **nixos/**: System configurations organized by platform (desktop/laptop) and host
- **editor/**: Text editor configurations (Doom Emacs, Neovim)
- **sway/**: Wayland compositor configuration for Linux systems
- **term/**: Terminal emulator configurations (kitty, ghostty, alacritty)
- **zsh/**: Shell configuration and custom functions

### Configuration Organization
- **Common configurations**: Shared between platforms in `nixos/common/`
  - `linux/`: NixOS-specific common settings (Wayland, Sway, audio via PipeWire)
  - `apple/`: macOS-specific common settings (Homebrew, system defaults)
- **Host-specific**: Individual machine configurations in `nixos/{desktop,laptop}/`
- **Platform separation**: Linux uses native package management, macOS uses Homebrew for GUI apps

### Key Components
- **Home Manager**: Manages user-level configurations and dotfiles
- **Sway**: Wayland compositor for Linux desktops with tiling window management
- **Aerospace**: Tiling window manager for macOS
- **Development tools**: Emacs (primary editor), Neovim, various LSPs and formatters

## Common Commands

### System Management
```bash
# NixOS systems
sudo nixos-rebuild switch

# macOS systems  
darwin-rebuild switch

# Development shell with build tools
nix develop
```

### Setup Commands
```bash
# Link configuration for NixOS
make link-nixos
# Or manually: sudo mv /etc/nixos /etc/nixos.old && sudo ln -s ~/_config /etc/nixos

# Link configuration for macOS
make link-macos  
# Or manually: sudo ln -s ~/_config /etc/nix-darwin
```

### Configuration Validation
```bash
# Check Nix syntax
nix flake check

# Format Nix files
nixfmt **/*.nix

# Lint YAML files (for sops secrets)
yamllint
```

## Host Configurations

### Linux Hosts
- **sophie**: Desktop system with ZFS storage pool
- **framework13**: Framework laptop
- **xps15-7590**: Dell XPS laptop with custom screen configuration
- **thinkpadz16**: ThinkPad laptop with Sway screen setup

### macOS Hosts  
- **m4mini**: Mac Mini desktop
- **mbp-inngest**: MacBook Pro (work machine)

## Development Environment

### Editors
- **Doom Emacs**: Primary editor with extensive language support, eglot LSP integration
- **Neovim**: Secondary editor with Lua configuration
- Development packages include LSPs for Nix, TypeScript, and other languages

### Languages & Tools
- **Nix**: Primary configuration language with nixd LSP
- **Python**: Configured with common packages (black, pytest, ipython)
- **Development tools**: ripgrep, fzf, git, tmux, various formatters

### Terminal Setup
- **Shell**: Zsh with antidote plugin manager, starship prompt
- **Terminal multiplexer**: tmux with custom configuration
- **File manager**: yazi, lf for terminal-based file management

## Notes Repository
The system automatically clones a private notes repository to `~/notes` on first setup if SSH authentication to GitHub is available.

## Secrets Management
Uses sops-nix for encrypted secrets, with age keys stored in `~/.config/sops/age/`.

## Editor Configuration Notes
- Doom Emacs config includes eglot setup with custom code action indicators
- Elixir development uses Lexical language server
- Rust development configured with rust-analyzer and clippy
- TypeScript configured with 2-space indentation