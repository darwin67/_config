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
zplug 'b4b4r07/enhancd', use:init.sh

zplug 'tmux-plugins/tpm'

# The platinum searcher
zplug 'monochromegane/the_platinum_searcher', as:command, rename-to:pt, from:gh-r

# ripgrep
zplug 'BurntSushi/ripgrep', as:command, rename-to:rg, from:gh-r, at:0.10.0

# bat
zplug 'sharkdp/bat', as:command, rename-to:bat, from:gh-r, use:"*x86_64*linux*"

# Ruby
zplug 'rbenv/rbenv', as:command, use:'bin/*', from:github
zplug 'rbenv/ruby-build', as:command, use:'bin/*', from:github, on:'rbenv/rbenv'

# Python
zplug 'pyenv/pyenv', as:command, use:'bin/*', from:github
zplug 'pyenv/pyenv', as:command, use:'plugins/python-build/bin/*', from:github
zplug 'pyenv/pyenv-virtualenv', as:command, use:'bin/*', from:github, on:'pyenv/pyenv'

# NodeJS
zplug 'nodenv/nodenv', as:command, use:'bin/*', from:github
zplug 'nodenv/node-build', as:command, use:'bin/*', from:github

# Golang
zplug 'syndbg/goenv', as:command, use:'bin/*', from:github
zplug 'syndbg/goenv', as:command, use:'plugins/go-build/bin/*', from:github

# Github
zplug 'github/hub', as:command, from:gh-r

# Peco
zplug 'peco/peco', as:command, from:gh-r

# Convenient stuff from oh-my-zsh
zplug 'plugins/git', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/github', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/tmux', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/cargo', from:oh-my-zsh, ignore:oh-my-zsh.sh, use:'_cargo'
zplug 'plugins/docker-compose', from:oh-my-zsh, ignore:oh-my-zsh.sh

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

# Initialize envs
eval "$(rbenv init -)"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
eval "$(nodenv init -)"
eval "$(goenv init -)"

if [[ -d $HOME/bin ]]; then
    PATH="$HOME/bin:${PATH}"
fi

if [ $commands[kubectl] ]; then
    source <(kubectl completion zsh)
fi

if [[ ! -d ${TPM_PLUGINS} ]]; then
    mkdir -p ${TPM_PLUGINS}
    ln -s "${ZPLUG_REPOS}/tmux-plugins/tpm" ${TPM_PLUGINS}
fi

# ================================================================================
#   Alias
# ================================================================================

alias rb='ruby'
alias be='bundle exec'
alias py='python'
alias C=clear
alias ls='ls --color'
alias ll='ls -lah'
alias c='cd ~'
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias emacs='emacs -nw'

if type nvim > /dev/null 2>&1 ; then
    alias vim='nvim'
fi

if type rg > /dev/null 2>&1 ; then
    alias grep='rg'
fi

# ================================================================================
#   Environment Variables
# ================================================================================

# You may need to manually set your language environment
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='emacs'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/"

# Neovim related
export PYENV_ROOT="$HOME/.pyenv"

# Golang
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$HOME/go/bin:$PATH"

# vagrant settings
if type libvirtd > /dev/null 2>&1 ; then
  export VAGRANT_DEFAULT_PROVIDER=libvirt
fi

# ================================================================================
#   Functions
# ================================================================================

function replace_in_file () {
    local current=$1
    local replacement=$2

    rg --files-with-matches "$current" | xargs sed -i "s/$current/$replacement/g"
}
