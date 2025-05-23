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

in { config, ... }: {
  imports = [ inputs.timewall.homeManagerModules.default ];

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

    sessionPath = [
      "${config.home.homeDirectory}/bin"
      "${config.home.homeDirectory}/.config/emacs/bin"
      "${config.home.homeDirectory}/.local/bin"
      "${config.home.homeDirectory}/.cargo/bin"
      "${config.home.homeDirectory}/.node/npm-pkgs/bin"
    ];

    sessionVariables = {
      EDITOR = "vim";
      SSH_KEY_PATH = "~/.ssh";

      # Python
      PY_COLORS = "1";

      # Wayland
      MOZ_ENABLE_WAYLAND = 1;

      CHROME_EXECUTABLE = "${pkgs.google-chrome}";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    };

    ## Files
    file = {
      # local bins
      "bin/.keep".text = "";

      # npm global install
      ".node/npm-pkgs/lib/.keep".text = "";
      ".node/npm-pkgs/bin/.keep".text = "";

      ".config/sops/age/.keep".text = "";

      # dot files
      ".gitconfig".source = "${self}/dots/.gitconfig";
      ".gitignore_global".source = "${self}/dots/.gitignore_global";
      ".pryrc".source = "${self}/dots/.pryrc";
      ".npmrc".source = "${self}/dots/.npmrc";

      # zsh
      ".config/zsh/functions".source = "${self}/zsh/zfunc";

      # editor
      ".doom.d".source = "${self}/editor/doom";
      ".config/nvim".source = "${self}/editor/nvim";

      # Sway
      ".config/sway/config".source = "${self}/sway/config";
      ".config/sway/config.d/zoom.conf".source =
        "${self}/sway/config.d/zoom.conf";

      ".config/waybar".source = "${self}/sway/waybar";
      ".config/wofi".source = "${self}/sway/wofi";
      ".config/wob".source = "${self}/sway/wob";
      ".config/mako".source = "${self}/sway/mako";
      ".config/satty".source = "${self}/sway/satty";

      # Terminal
      ".tmux.conf".source = "${self}/term/.tmux.conf";
      ".alacritty.toml".source = "${self}/term/.alacritty.toml";
      ".config/kitty".source = "${self}/term/kitty";
      ".config/ghostty".source = "${self}/term/ghostty";

      # Flags
      ".config/brave-flags.conf".text = chromeFlags;
      ".config/chrome-flags.conf".text = chromeFlags;
      ".config/discord-flags.conf".text = chromeFlags;
      ".config/zoom-flags.conf".text = chromeFlags;

      # Misc
      "Notes/.keep".text = "";
      "Pictures/Screenshots/.keep".text = "";
    } // additionalFiles;
  };

  programs = {
    home-manager.enable = true;
    lf.enable = true;

    zsh = {
      enable = true;
      enableCompletion = true;
      # enableAutosuggestions = true;
      syntaxHighlighting.enable = true;
      autosuggestion.enable = true;
      history.size = 10000;

      shellAliases = {
        ll = "ls -lah";
        pbcopy = "wl-copy";
        pbpaste = "wl-paste";
        emacs = "emacs -nw";

        # Ruby
        be = "bundle exec";

        # Git
        gst = "git status";
        gco = "git checkout";
        gca = "git commit -v -a";
        "gca!" = "git commit -v -a --amend";
        gp = "git push";
        gr = "git remote";
        gb = "git branch";
        glog =
          "git log --oneline --graph --all --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'";
        gsta = "git stash push";
        gstp = "git stash pop";

        vim = "nvim";
        grep = "rg";
      };

      # zsh plugin management
      antidote = {
        enable = true;
        useFriendlyNames = true;
        plugins = [
          "zsh-users/zsh-history-substring-search"
          "zsh-users/zsh-syntax-highlighting"
          # "zsh-users/zsh-completions"
          # "zsh-users/zsh-autosuggestions"
          # mafredri/zsh-async
          "supercrabtree/k"
          "mattmc3/zfunctions"
          # zdharma-continuum/fast-syntax-highlighting kind:defer

          # NOTE: just need the repo. the rest is done when the shell inits in .zshrc
          "tmux-plugins/tpm kind:path"

          # Utilities
          # belak/zsh-utils path:editor
          # belak/zsh-utils path:history
          "belak/zsh-utils path:prompt"
          "belak/zsh-utils path:utility"
          # belak/zsh-utils path:completion

          # Convenient stuff from oh-my-zsh
          "ohmyzsh/ohmyzsh path:plugins/tmux"
          "ohmyzsh/ohmyzsh path:plugins/colored-man-pages"

          # Convenient stuff from prezto
          "sorin-ionescu/prezto path:modules/editor"
          "sorin-ionescu/prezto path:modules/completion"
          "sorin-ionescu/prezto path:modules/history"

          # Theme
          # sindresorhus/pure kind:fpath
          "chisui/zsh-nix-shell"
        ];
      };
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
