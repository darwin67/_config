{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "darwin";
  home.homeDirectory = "/home/darwin";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "22.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # Common
    pkgs.fwupd
    pkgs.jq
    pkgs.yq
    pkgs.neovim
    pkgs.vim
    pkgs.git
    pkgs.fd
    pkgs.tmux
    pkgs.ripgrep
    # pkgs.alacritty

    ## System
    # Sway
    pkgs.sway
    pkgs.swaybg
    pkgs.swayimg
    pkgs.swaylock-effects
    pkgs.swayidle
    pkgs.swaynotificationcenter
    pkgs.wob
    pkgs.wev
    # Screenshots
    pkgs.grim
    pkgs.slurp
    # Recorder
    pkgs.wf-recorder
    # Controls
    pkgs.playerctl
    pkgs.brightnessctl
    pkgs.sirula # app launcher

    # Shell
    pkgs.starship

    # Fonts
    pkgs.nerdfonts
    pkgs.noto-fonts
    pkgs.noto-fonts-emoji
    pkgs.noto-fonts-cjk-sans
    pkgs.noto-fonts-cjk-serif
    pkgs.font-awesome

    # Files
    pkgs.xfce.thunar
    pkgs.xfce.tumbler
    pkgs.xfce.thunar-volman
    pkgs.xfce.thunar-archive-plugin
    pkgs.xfce.thunar-media-tags-plugin

    # Battery management
    pkgs.tlp

    # Programming language setup
    #   Ruby, Python, Erlang
    pkgs.libffi
    pkgs.libyaml
    pkgs.openssl
    pkgs.zlib
    pkgs.xz
    # https://github.com/asdf-vm/asdf-erlang
    pkgs.ncurses
    pkgs.libGLU
    pkgs.mesa
    pkgs.libpng
    pkgs.libssh
    pkgs.unixODBC
    pkgs.libxslt
    pkgs.fop

    # Editor
    pkgs.nixfmt

    # Language Servers
    pkgs.gopls
    pkgs.rust-analyzer
    pkgs.nodePackages.typescript-language-server
    pkgs.nodePackages.vscode-json-languageserver
    pkgs.nodePackages.yaml-language-server
    pkgs.nodePackages.vscode-css-languageserver-bin
    pkgs.nodePackages.vscode-html-languageserver-bin
    pkgs.elixir-ls

    ## Human language
    # pkgs.fcitx5
    # pkgs.fcitx5-mozc
    # pkgs.fcitx5-rime
    # pkgs.fcitx5-configtool
    # pkgs.fcitx5-gtk
    # pkgs.fcitx5-qt

    ## Social
    pkgs.discord

    ## Security
    # pkgs._1password # not working?

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;
    # ".alacritty.yml" = _config/.alacritty.yml;
    # ".pryrc" = _config/.pryrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/darwin/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Services
  # services = { tlp = { enable = true; }; };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
