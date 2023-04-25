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
    pkgs.hub
    pkgs.fd
    pkgs.tmux
    pkgs.ripgrep
    pkgs.bat
    pkgs.curlie
    pkgs.wget
    pkgs.sqlite

    ## System
    # Shell
    pkgs.starship

    # Programming language setup
    #   Ruby, Python, Erlang
    pkgs.openssl
    # Golang
    pkgs.go
    pkgs.golangci-lint
    pkgs.gotests
    # Ruby
    pkgs.ruby_3_2
    # Python
    pkgs.python310Full
    pkgs.python310Packages.pip
    # Node
    pkgs.yarn
    pkgs.nodejs-18_x
    # Elixir
    pkgs.elixir
    # Erlang
    pkgs.erlang
    # Flutter / Dart
    pkgs.flutter
    pkgs.dart

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
    pkgs.erlang-ls

    # Linters / Formatters
    pkgs.yamllint
    pkgs.sqlint

    # Tools
    pkgs.terraform
    pkgs.tflint
    pkgs.protobuf
    pkgs.awscli2
    pkgs.kubectl

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
    # dotfiles
    ".alacritty.yml" = { source = ~/_config/.alacritty.yml; };
    # ".asdfrc" = { source = ~/_config/.asdfrc; };
    ".gitconfig" = { source = ~/_config/.gitconfig; };
    ".gitignore_global" = { source = ~/_config/.gitignore_global; };
    ".pryrc" = { source = ~/_config/.pryrc; };
    ".tmux.conf" = { source = ~/_config/.tmux.conf; };
    ".zlogin" = { source = ~/_config/.zlogin; };
    ".zsh_plugins.txt" = { source = ~/_config/.zsh_plugins.txt; };
    ".zshrc" = { source = ~/_config/.zshrc; };

    ".config/nvim/init.vim" = { source = ~/_config/init.vim; };
    # Directory
    ".doom.d" = { source = ~/_config/.doom.d; };

    # Sway
    ".config/sway/config" = { source = ~/_config/sway.conf; };
    ".config/starship.toml" = { source = ~/_config/starship.toml; };
    ".config/waybar/config" = { source = ~/_config/waybar/config.json; };
    ".config/waybar/mediaplayer.sh" = {
      source = ~/_config/waybar/mediaplayer.sh;
    };
    ".config/waybar/style.css" = { source = ~/_config/waybar/style.css; };
    ".config/wofi" = { source = ~/_config/wofi; };
    ".config/wob" = { source = ~/_config/wob; };
    ".config/sirula" = { source = ~/_config/sirula; };

    # Flags
    ".config/chromium-flags.conf" = {
      text = ''
        --enable-features=UseOzonePlatform
        --ozone-platform=wayland
      '';
    };
    ".config/brave-flags.conf" = { source = ~/.config/chromium-flags.conf; };
    ".config/chrome-flags.conf" = { source = ~/.config/chromium-flags.conf; };
    ".config/discord-flags.conf" = { source = ~/.config/chromium-flags.conf; };
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  # or
  #  /etc/profiles/per-user/darwin/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "emacsclient";
    SSH_KEY_PATH = "~/.ssh/";
    KERL_BUILD_DOCS = "yes";

    # Python / Ansible
    PY_COLORS = "1";
    ANSIBLE_FORCE_COLOR = "1";

    # Wayland
    MOZ_ENABLE_WAYLAND = 1;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
