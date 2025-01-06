{ pkgs, self, stateVersion, ... }:

{
  nixpkgs = { config = { allowUnfree = true; }; };

  home = {
    username = "darwin";
    homeDirectory = "/home/darwin";
    stateVersion = stateVersion;
    pointerCursor = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
      size = 24;
      x11 = {
        enable = true;
        defaultCursor = "Adwaita";
      };
    };

    packages = with pkgs; [ starship ];

    sessionVariables = {
      EDITOR = "vim";
      SSH_KEY_PATH = "~/.ssh";

      # Python
      PY_COLORS = "1";

      # Wayland
      MOZ_ENABLE_WAYLAND = 1;
    };

    ## Files
    file = {
      ".gitconfig" = { source = "${self}/.gitconfig"; };
      ".gitignore_global" = { source = "${self}/.gitignore_global"; };
      ".pryrc" = { source = "${self}/.pryrc"; };
      ".npmrc" = { source = "${self}/.npmrc"; };
      ".tmux.conf" = { source = "${self}/.tmux.conf"; };
      ".zlogin" = { source = "${self}/.zlogin"; };
      ".zsh_plugins.txt" = { source = "${self}/.zsh_plugins.txt"; };
      ".zshrc" = { source = "${self}/.zshrc"; };

      ".config/nvim/init.vim" = { source = "${self}/init.vim"; };
      ".config/nvim/lua" = { source = "${self}/nvim"; };
      # # Directory
      ".doom.d" = { source = "${self}/.doom.d"; };
      ".config/zsh/functions" = { source = "${self}/zfunc"; };

      # # Sway
      ".config/sway" = { source = "${self}/sway"; };
      ".config/waybar/config" = { source = "${self}/waybar/config.json"; };
      ".config/waybar/style.css" = { source = "${self}/waybar/style.css"; };
      ".config/wofi" = { source = "${self}/wofi"; };
      ".config/wob" = { source = "${self}/wob"; };

      # Terminal
      ".alacritty.toml" = { source = "${self}/.alacritty.toml"; };
      ".config/kitty" = { source = "${self}/kitty"; };

      # Flags
      ".config/brave-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
      ".config/chrome-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
      ".config/discord-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
      ".config/zoom-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
    };
  };

  programs = {
    home-manager.enable = true;
    lf.enable = true;

    zsh = {
      enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      history.size = 10000;
    };

    direnv = {
      enable = true;
      nix-direnv = { enable = true; };
    };

    starship = {
      enable = true;

      settings = {
        add_newline = true;
        aws.disabled = true;
        gcloud.disabled = true;
        shlvl.disabled = true;
        singularity.disabled = true;
        kubernetes.disabled = true;
        vcsh.disabled = true;
        fossil_branch.disabled = true;
        fossil_metrics.disabled = true;

        hg_branch.disabled = true;
        pijul_channel.disabled = true;
        docker_context.disabled = true;
      };
    };
  };
}
