{ pkgs, self, inputs, username, wallpaperTheme, stateVersion, additionalFiles
, ... }:

let
  timed-wallpaper = import ./wallpaper.nix {
    inherit pkgs;
    theme = wallpaperTheme;
  };

  chromeFlags = ''
    --enable-features=UseOzonePlatform
    --ozone-platform=wayland
  '';

in {
  imports = [ inputs.timewall.homeManagerModules.default ];

  nixpkgs = { config = { allowUnfree = true; }; };

  home = {
    username = username;
    homeDirectory = "/home/${username}";
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
      # dot files
      ".gitconfig" = { source = "${self}/dots/.gitconfig"; };
      ".gitignore_global" = { source = "${self}/dots/.gitignore_global"; };
      ".pryrc" = { source = "${self}/dots/.pryrc"; };
      ".npmrc" = { source = "${self}/dots/.npmrc"; };
      ".tmux.conf" = { source = "${self}/dots/.tmux.conf"; };

      # zsh
      ".zlogin" = { source = "${self}/zsh/.zlogin"; };
      ".zsh_plugins.txt" = { source = "${self}/zsh/.zsh_plugins.txt"; };
      ".zshrc" = { source = "${self}/zsh/.zshrc"; };
      ".config/zsh/functions" = { source = "${self}/zsh/zfunc"; };

      # editor
      ".doom.d" = { source = "${self}/.doom.d"; };
      ".config/nvim" = { source = "${self}/nvim"; };

      # Sway
      ".config/sway/config" = { source = "${self}/sway/config"; };
      ".config/sway/config.d/zoom.conf" = {
        source = "${self}/sway/config.d/zoom.conf";
      };

      ".config/waybar/config" = { source = "${self}/waybar/config.json"; };
      ".config/waybar/style.css" = { source = "${self}/waybar/style.css"; };
      ".config/wofi" = { source = "${self}/wofi"; };
      ".config/wob" = { source = "${self}/wob"; };

      # Terminal
      ".alacritty.toml" = { source = "${self}/dots/.alacritty.toml"; };
      ".config/kitty" = { source = "${self}/kitty"; };

      # Flags
      ".config/brave-flags.conf" = { text = chromeFlags; };
      ".config/chrome-flags.conf" = { text = chromeFlags; };
      ".config/discord-flags.conf" = { text = chromeFlags; };
      ".config/zoom-flags.conf" = { text = chromeFlags; };
    } // additionalFiles;
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

  services = {
    timewall = {
      enable = true;
      wallpaperPath = "${timed-wallpaper}";
      config = {
        daemon = { update_interval_seconds = 600; };
        setter = {
          # NOTE: based on
          # https://docs.rs/wallpape-rs/latest/wallpape_rs/
          command = [ "swaybg" "--mode" "fill" "--image" "%f" ];
        };
      };
    };
  };
}
