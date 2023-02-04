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

# Use friendly path names for antidote
zstyle ':antidote:bundle' use-friendly-names 'yes'

# source antidote
source ${ZDOTDIR:-~}/.antidote/antidote.zsh

# zsh autoload
autoload -Uz compinit && compinit

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
antidote load

# Load theme
autoload -Uz promptinit && promptinit && prompt pure

# ================================================================================
#   Path and sourcing
# ================================================================================

# Terminal emacs binding
zstyle ':prezto:module:editor' key-bindings 'emacs'

if [[ -d $HOME/bin ]]; then
    export PATH="$HOME/bin:${PATH}"
fi

if [ $commands[kubectl] ]; then
    source <(kubectl completion zsh)
fi

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

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
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

alias backup="restic backup --verbose --exclude-file=$HOME/_config/etc/backup-excludes.txt backup ~/"

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

if type nvim > /dev/null 2>&1 ; then
    alias vim='nvim'
fi

if type rg > /dev/null 2>&1 ; then
    alias grep='rg'
fi

if type go > /dev/null 2>&1 ; then
    export GOPATH=$(go env GOPATH)
    export PATH="${GOPATH}/bin:${PATH}"
fi

if type ssh-agent > /dev/null 2>&1 ; then
    eval $(ssh-agent) > /dev/null 2>&1
fi

# ================================================================================
#   Environment Variables
# ================================================================================

# Location for zfunctions plugin
export ZFUNCDIR="$CONFIG/zfunc"

# Preferred editor for local and remote sessions
export EDITOR='emacsclient'

# ssh
export SSH_KEY_PATH="~/.ssh/"

# Allow path definition in go get
export GO111MODULE=on

# Elixir & Erlang settings
export ELS_INSTALL_PREFIX="$HOME/.lsp/elixir"
export PATH="$HOME/.lsp/elixir:$PATH"
export KERL_BUILD_DOCS=yes

# Dart & Flutter
export PATH="$HOME/Android/Sdk/tools/bin:$PATH"

## export google chrome bin for flutter usage
if type google-chrome-stable > /dev/null 2>&1 ; then
  export CHROME_EXECUTABLE=$(which google-chrome-stable)
fi

# Python & Ansible
export PY_COLORS='1' ANSIBLE_FORCE_COLOR='1'

# Wayland
export MOZ_ENABLE_WAYLAND=1
