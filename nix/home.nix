{ config, pkgs, ... }:

let unstable = import <nixpkgs> { };

in {
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
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # Common
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
    pkgs.sqlite
    pkgs.tree
    pkgs.pet
    pkgs.direnv
    pkgs.nix-direnv
    pkgs.openssl
    pkgs.bottom
    pkgs.starship # shell

    # Languages
    pkgs.elixir
    pkgs.ruby
    (pkgs.python311.withPackages (p:
      with p; [
        pip
        grip
        pytest
        pyflakes
        nose
        isort
        cffi
        ipython
        ansible-core
      ]))

    # Node
    pkgs.yarn
    pkgs.nodejs-18_x

    # Editor
    pkgs.nixfmt
    pkgs.editorconfig-core-c
    pkgs.black
    pkgs.gnuplot
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.glslang
    pkgs.maim
    pkgs.scrot
    pkgs.nodePackages.prettier
    pkgs.nodePackages.js-beautify
    pkgs.stylelint
    pkgs.html-tidy
    pkgs.markdownlint-cli

    # Language Servers
    pkgs.vimPlugins.vim-lsp
    pkgs.nil

    # Tools
    pkgs.awscli2

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
    ".alacritty.toml" = { source = ~/_config/.alacritty.toml; };
    ".gitconfig" = { source = ~/_config/.gitconfig; };
    ".gitignore_global" = { source = ~/_config/.gitignore_global; };
    ".pryrc" = { source = ~/_config/.pryrc; };
    ".npmrc" = { source = ~/_config/.npmrc; };
    ".tmux.conf" = { source = ~/_config/.tmux.conf; };
    ".zlogin" = { source = ~/_config/.zlogin; };
    ".zsh_plugins.txt" = { source = ~/_config/.zsh_plugins.txt; };
    ".zshrc" = { source = ~/_config/.zshrc; };

    ".config/nvim/init.vim" = { source = ~/_config/init.vim; };
    ".config/nvim/lua" = { source = ~/_config/nvim; };
    # Directory
    ".doom.d" = { source = ~/_config/.doom.d; };
    ".config/zsh/functions" = { source = ~/_config/zfunc; };

    # Sway
    ".config/sway" = { source = ~/_config/sway; };
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
    ".config/discord-flags.conf" = {
      text = ''
        --enable-features=UseOzonePlatform
        --ozone-platform=wayland
        --enable-features=WebRTCPipeWireCapturer
      '';
    };
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
    # EDITOR = "emacsclient";
    EDITOR = "vim";
    SSH_KEY_PATH = "~/.ssh/";
    KERL_BUILD_DOCS = "yes";

    # Python / Ansible
    PY_COLORS = "1";
    ANSIBLE_FORCE_COLOR = "1";

    # Wayland
    MOZ_ENABLE_WAYLAND = 1;
  };

  # Let Home Manager install and manage itself.
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      # autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      history.size = 10000;
    };
    home-manager = { enable = true; };
    direnv = {
      enable = true;
      nix-direnv = { enable = true; };
    };
  };
}
