#!/usr/local/bin/zsh
#
# ===
#   Zsh configs
#

# ================================================================================
#   Antidote setup
# ================================================================================

# Download antidote if not available
antidote_dir=${ZDOTDIR:-~}/.antidote
plugins_txt=${ZDOTDIR:-~}/.zsh_plugins.txt
static_file=${ZDOTDIR:-~}/.zsh_plugins.zsh

if [[ ! $static_file -nt $plugins_txt ]]; then
  [[ -e $antidote_dir ]] || git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
fi

# Some useful variables
export CONFIG="$HOME/_config"
export TPM_PLUGINS="${HOME}/.tmux/plugins"
export ANTIDOTE_CACHE="${HOME}/.cache/antidote"

# Nix related
# export LC_ALL=C
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

# Use friendly path names for antidote
zstyle ':antidote:bundle' use-friendly-names 'yes'

# source antidote
source ${ZDOTDIR:-~}/.antidote/antidote.zsh

# zsh autoload
autoload -Uz compinit && compinit

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
antidote load

# ================================================================================
#   Path and sourcing
# ================================================================================

# Terminal emacs binding
zstyle ':prezto:module:editor' key-bindings 'emacs'

if [[ -d $HOME/bin ]]; then
  export PATH="$HOME/bin:${PATH}"
fi

# if [ $commands[kubectl] ]; then
#   source <(kubectl completion zsh)
# fi

if [[ ! -d $HOME/.node/npm-pkgs ]]; then
  mkdir -p $HOME/.node/npm-pkgs/{lib,bin}
fi
export PATH="$HOME/.node/npm-pkgs/bin:$PATH"

if [[ ! -d ${TPM_PLUGINS} ]]; then
  mkdir -p ${TPM_PLUGINS}
  ln -s "${ANTIDOTE_CACHE}/tmux-plugins/tpm" ${TPM_PLUGINS}
fi

if [[ -d $HOME/.cargo ]]; then
  export PATH="$HOME/.cargo/bin:$PATH"
fi

if [[ -d $HOME/.emacs.d/bin ]]; then
  export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [[ -d $HOME/.config/emacs/bin ]]; then
  export PATH="$HOME/.config/emacs/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
  export PATH="$HOME/.local/bin:$PATH"
fi

# ================================================================================
#   Alias
# ================================================================================

# Shell
alias ll='ls -lah'
alias pbcopy='wl-copy'
alias pbpaste='wl-paste'
alias emacs='emacs -nw'
alias be='bundle exec'

# alias backup="restic backup --verbose --exclude-file=$HOME/_config/etc/backup-excludes.txt backup ~/"

# Git
alias gst='git status'
alias gco='git checkout'
alias gca='git commit -v -a'
alias gca!='git commit -v -a --amend'
alias gp='git push'
alias gr='git remote'
alias gb='git branch'
alias glog="git log --oneline --graph --all --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'"
alias gsta='git stash push'
alias gstp='git stash pop'

if type nvim >/dev/null 2>&1; then
  alias vim='nvim'
fi

if type rg >/dev/null 2>&1; then
  alias grep='rg'
fi

if type go >/dev/null 2>&1; then
  export GOPATH=$(go env GOPATH)
  export PATH="${GOPATH}/bin:${PATH}"
fi

if type fly >/dev/null 2>&1; then
  export FLYCTL_INSTALL="$HOME/.fly"
  export PATH="$FLYCTL_INSTALL/bin:$PATH"
fi

# ================================================================================
#   Environment Variables
# ================================================================================

# Elixir & Erlang settings
# export ELS_INSTALL_PREFIX="$HOME/.lsp/elixir"
# export PATH="$HOME/.lsp/elixir:$PATH"
# export KERL_BUILD_DOCS=yes

## export google chrome bin for flutter usage
if type google-chrome-stable >/dev/null 2>&1; then
  export CHROME_EXECUTABLE=$(which google-chrome-stable)
fi

# Nix locale archive
if type nix-env >/dev/null 2>&1; then
  # Install with `nix-env -iA nixpkgs.glibcLocales`
  export LOCALE_ARCHIVE="$(nix-env --installed --no-name --out-path --query glibc-locales)/lib/locale/locale-archive"
  # If $XDG_DATA_DIRS is either not set or empty, a value equal to /usr/local/share/:/usr/share/ should be used.
  export XDG_DATA_DIRS=$HOME/.nix-profile/share:$XDG_DATA_DIRS
fi

if type direnv >/dev/null 2>&1; then
  # Hook direnv
  eval "$(direnv hook zsh)"
fi

if type starship >/dev/null 2>&1; then
  # Initialize starship shell prompt
  eval "$(starship init zsh)"
fi
