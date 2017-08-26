#!/usr/local/bin/zsh
#
# ===
#   Zsh configs
#

# Path to configuration directory
export CONFIG="$HOME/_config"

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

zplug 'darwin67/tmux-powerline', defer:3, at:my

# The platinum searcher
zplug 'monochromegane/the_platinum_searcher', as:command, rename-to:pt, from:gh-r

# Ruby
zplug 'rbenv/rbenv', as:command, use:'bin/*', from:github
zplug 'rbenv/ruby-build', as:command, use:'bin/*', from:github, on:'rbenv/rbenv'

# Python
zplug 'pyenv/pyenv', as:command, use:'bin/*', from:github
zplug 'pyenv/pyenv', as:command, use:'plugins/python-build/bin/*', from:github

# NodeJS
zplug 'nodenv/nodenv', as:command, use:'bin/*', from:github
zplug 'nodenv/node-build', as:command, use:'bin/*', from:github

# Github
zplug 'github/hub', as:command, from:gh-r

# Peco
zplug 'peco/peco', as:command, from:gh-r

# Convenient stuff from oh-my-zsh
zplug 'plugins/git', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/github', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/tmux', from:oh-my-zsh, ignore:oh-my-zsh.sh
zplug 'plugins/cargo', from:oh-my-zsh, ignore:oh-my-zsh.sh, use:'_cargo'

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
eval "$(nodenv init -)"

if [[ -d $HOME/.cargo ]]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# ================================================================================
#   Alias
# ================================================================================

alias clean_git_branches='git gc --prune=now && git remote prune origin'
alias rb='ruby'
alias be='bundle exec'
alias py='python'
alias C=clear
alias ls=k
alias ll='ls -a'
alias c='cd ~'

if type nvim > /dev/null 2>&1 ; then
    alias vim='nvim'
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

export TMUX_PL="$ZPLUG_REPOS/darwin67/tmux-powerline"
export TMUX_POWERLINE_THEME_DEFAULT="my"

# ================================================================================
#   Functions
# ================================================================================
function link_config_files() {
    local files=( .zshrc .zlogin .sqliterc .ctags .railsrc .rspec .spacemacs .irbrc .pryrc .gemrc .rubocop.yml .gitconfig .gitignore_global )

    for file in ${files[@]}; do
        ln -sf $CONFIG/$file $HOME/$file
    done

    if [[ "$OSTYPE" == "linux-gnu" ]]; then
        ln -sf $CONFIG/.tmux-linux.conf $HOME/.tmux.conf
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        ln -sf $CONFIG/.tmux-osx.conf $HOME/.tmux.conf
    fi
}
