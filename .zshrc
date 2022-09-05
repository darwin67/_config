#!/usr/local/bin/zsh
#
# ===
#   Zsh configs
#

# Some useful variables
export CONFIG="$HOME/_config"
export TPM_PLUGINS="${HOME}/.tmux/plugins"

# ================================================================================
#   zplug
# ================================================================================

if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug $HOME/.zplug
    source $HOME/.zplug/init.zsh && zplug update --self
fi

source $HOME/.zplug/init.zsh

# zplug 'zplug/zplug', hook-build:'zplug --self-manage'

zplug 'zsh-users/zsh-history-substring-search'
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'zsh-users/zsh-completions'
zplug 'zsh-users/zsh-autosuggestions'
zplug 'mafredri/zsh-async'
zplug 'supercrabtree/k'

zplug 'tmux-plugins/tpm'

# The platinum searcher
zplug 'monochromegane/the_platinum_searcher', as:command, rename-to:pt, from:gh-r

# ripgrep
zplug 'BurntSushi/ripgrep', as:command, rename-to:rg, from:gh-r

# bat
zplug 'sharkdp/bat', as:command, rename-to:bat, from:gh-r, use:"*x86_64*linux*"

# Language manager
zplug 'asdf-vm/asdf', from:github

# Github
zplug 'github/hub', as:command, from:gh-r

# Peco
zplug 'peco/peco', as:command, from:gh-r

# Convenient stuff from oh-my-zsh
zplug 'plugins/git', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/github', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/tmux', from:oh-my-zsh, ignore:oh-my-zsh.sh

# Convenient stuff from prezto
zplug 'modules/editor', from:prezto
zplug 'modules/completion', from:prezto
zplug 'modules/history', from:prezto

# Theme
zplug 'sindresorhus/pure', use:pure.zsh, from:github, as:theme

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load # --verbose

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
    ln -s "${ZPLUG_REPOS}/tmux-plugins/tpm" ${TPM_PLUGINS}
fi

if [[ -d $ZPLUG_REPOS/asdf-vm/asdf ]]; then
    . $ZPLUG_REPOS/asdf-vm/asdf/asdf.sh
    # . $ZPLUG_REPOS/asdf-vm/asdf/completions/asdf.bash
fi

if [[ -d $HOME/.cargo ]]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

if [[ -d $HOME/.emacs.d/bin ]]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
    alias doom_compile="doom compile :core modules/{completion,input,ui,checkers,emacs}"
fi

# ================================================================================
#   Alias
# ================================================================================

alias rb='ruby'
alias be='bundle exec'
alias py='python'
alias ls='ls --color'
alias ll='ls -lah'
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias emacs='emacs -nw'
alias check_firmware="fwupdmgr get-updates"
alias update_firmware="fwupdmgr update"
alias gcst="git-crypt status"

alias backup="restic backup --verbose --exclude-file=$HOME/_config/etc/backup-excludes.txt backup ~/"

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

# ================================================================================
#   Environment Variables
# ================================================================================

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

# ================================================================================
#   Functions
# ================================================================================

function replace_in_file () {
    local current=$1
    local replacement=$2
    rg --files-with-matches "$current" | xargs sed -i "s/$current/$replacement/g"
}

function prev() {
  PREV=$(fc -lrn | head -n 1)
  sh -c "pet new `printf %q "$PREV"`"
}

# NOTE: run this command before installing
# * ibus-mozc
# * mozc-ut
# This is for direct access to python3 for bazel builds
function bypass_asdf() {
  export PATH="/usr/bin:$PATH"
}
