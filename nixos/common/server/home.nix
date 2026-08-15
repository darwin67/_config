{
  pkgs,
  self,
  username,
  stateVersion,
  home-manager,
  additionalFiles,
  ...
}:

{ config, ... }:
{
  imports = [ ../gpg.nix ];

  home = {
    username = username;
    homeDirectory = "/home/${username}";
    stateVersion = stateVersion;

    packages = with pkgs; [
      starship
      starship-jj
    ];

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
      PY_COLORS = "1";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    };

    file = {
      "bin/.keep".text = "";
      ".node/npm-pkgs/lib/.keep".text = "";
      ".node/npm-pkgs/bin/.keep".text = "";
      ".config/sops/age/.keep".text = "";

      # Amp
      ".config/amp/settings.json".text = builtins.toJSON {
        "amp.git.commit.ampThread.enabled" = false;
        "amp.git.commit.coauthor.enabled" = false;
        "amp.updates.mode" = "disabled";
      };

      ".gitconfig".source = "${self}/dots/.gitconfig";
      ".gitignore_global".source = "${self}/dots/.gitignore_global";
      ".config/jj/config.toml" = {
        force = true;
        source = "${self}/dots/jj/config.toml";
      };
      ".pryrc".source = "${self}/dots/.pryrc";
      ".npmrc".source = "${self}/dots/.npmrc";

      ".config/zsh/functions".source = "${self}/zsh/zfunc";
      ".doom.d".source = "${self}/editor/doom";
      ".config/nvim".source = "${self}/editor/nvim";
      ".tmux.conf".source = "${self}/term/.tmux.conf";
    }
    // additionalFiles;

    activation = {
      cloneNotesRepo = home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -d "$HOME/notes" ]; then
          if (PATH=${pkgs.openssh}/bin:$PATH ssh -T git@github.com -o ConnectTimeout=5 -o StrictHostKeyChecking=no -o BatchMode=yes 2>&1 || true) | grep -q "successfully authenticated"; then
            PATH=${pkgs.openssh}/bin:$PATH ${pkgs.git}/bin/git clone git@github.com:darwin67/notes.git "$HOME/notes"
          fi
        fi
      '';
    };
  };

  programs = {
    home-manager.enable = true;
    lf.enable = true;

    zsh = {
      enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      autosuggestion.enable = true;
      history.size = 10000;

      shellAliases = {
        ll = "ls -lah";
        emacs = "emacs -nw";
        be = "bundle exec";
        gst = "git status";
        gco = "git checkout";
        gca = "git commit -v -a";
        "gca!" = "git commit -v -a --amend";
        gp = "git push";
        gr = "git remote";
        gb = "git branch";
        glog = "git log --oneline --graph --all --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'";
        gsta = "git stash push";
        gstp = "git stash pop";
        vim = "nvim";
        grep = "rg";
      };

      antidote = {
        enable = true;
        useFriendlyNames = true;
        plugins = [
          "zsh-users/zsh-history-substring-search"
          "zsh-users/zsh-syntax-highlighting"
          "supercrabtree/k"
          "mattmc3/zfunctions"
          "tmux-plugins/tpm kind:path"
          "belak/zsh-utils path:prompt"
          "belak/zsh-utils path:utility"
          "ohmyzsh/ohmyzsh path:plugins/tmux"
          "ohmyzsh/ohmyzsh path:plugins/colored-man-pages"
          "sorin-ionescu/prezto path:modules/editor"
          "sorin-ionescu/prezto path:modules/completion"
          "sorin-ionescu/prezto path:modules/history"
          "chisui/zsh-nix-shell"
        ];
      };
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    starship = {
      enable = true;
      settings = {
        format = "$username$hostname$localip$shlvl$singularity$kubernetes$nats$directory$vcsh$fossil_branch$fossil_metrics\${custom.git_branch}\${custom.git_commit}\${custom.git_state}\${custom.git_metrics}\${custom.git_status}\${custom.jj}$all";
        add_newline = true;
        custom.jj = {
          command = "prompt";
          format = "$output ";
          ignore_timeout = true;
          shell = [
            "starship-jj"
            "--ignore-working-copy"
            "starship"
          ];
          use_stdin = false;
          when = true;
        };
        git_branch.disabled = true;
        custom.git_branch = {
          command = "starship module git_branch";
          description = "Only show git_branch outside jj repositories";
          require_repo = true;
          style = "";
          when = "! jj --ignore-working-copy root";
        };
        git_commit.disabled = true;
        custom.git_commit = {
          command = "starship module git_commit";
          description = "Only show git_commit outside jj repositories";
          require_repo = true;
          style = "";
          when = "! jj --ignore-working-copy root";
        };
        git_state.disabled = true;
        custom.git_state = {
          command = "starship module git_state";
          description = "Only show git_state outside jj repositories";
          require_repo = true;
          style = "";
          when = "! jj --ignore-working-copy root";
        };
        git_metrics.disabled = true;
        custom.git_metrics = {
          command = "starship module git_metrics";
          description = "Only show git_metrics outside jj repositories";
          require_repo = true;
          style = "";
          when = "! jj --ignore-working-copy root";
        };
        git_status.disabled = true;
        custom.git_status = {
          command = "starship module git_status";
          description = "Only show git_status outside jj repositories";
          require_repo = true;
          style = "";
          when = "! jj --ignore-working-copy root";
        };
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
