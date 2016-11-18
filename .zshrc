#!/usr/local/bin/zsh
# ================================================================================
#   Basic Configutations
# ================================================================================

# Path to configuration directory
export CONFIG="$HOME/_config"

# Path to your oh-my-zsh installation.
# export ZSH=$CONFIG/modules/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="custom"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=7

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM="$CONFIG/custom"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# plugins=(emacs git github gem rails pip tmux tmux-cssh zsh_reload) # aws

source $ZSH/oh-my-zsh.sh

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# The current OS
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export OS="Linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export OS="Mac"
fi

# ================================================================================
#   Paths
# ================================================================================

# User configuration
# Paths
export PATH="/bin:$PATH"
export PATH="/sbin:$PATH"
export PATH="/usr/bin:$PATH"
export PATH="/usr/sbin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/bin:$PATH"

if [[ $OS == "Mac" ]]; then
    export PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
    export PATH="/opt/local/bin:$PATH"
    export PATH="/opt/chefdk/bin:$PATH"
elif [[ $OS == "Linux" ]]; then
    export PATH="/usr/games:$PATH"
    export PATH="/usr/local/games:$PATH"
fi


export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
export MANPATH="/usr/local/man:$MANPATH"

# ================================================================================
#   version controls
# ================================================================================

function init_rbenv()  { eval "$(rbenv init -)" }
function init_pyenv()  { eval "$(pyenv init -)" }
function init_goenv()  { eval "$(goenv init -)" }
function init_nodenv() { eval "$(nodenv init -)" }

# Ruby
export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/shims:$PATH"
export PATH="$RBENV_ROOT/bin:$PATH"
if [ -d "$RBENV_ROOT" ]; then
    init_rbenv
else
    git clone https://github.com/rbenv/rbenv.git $RBENV_ROOT && \
        git clone https://github.com/rbenv/ruby-build.git $RBENV_ROOT/plugins/ruby-build && \
        git clone https://github.com/rkh/rbenv-update.git $RBENV_ROOT/plugins/rbenv-update && \
        init_rbenv
fi

# Python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
export PATH="$PYENV_ROOT/bin:$PATH"
if [[ -d $PYENV_ROOT ]]; then
    init_pyenv
else
    git clone https://github.com/yyuu/pyenv.git $PYENV_ROOT && \
        git clone git://github.com/yyuu/pyenv-update.git $PYENV_ROOT/plugins/pyenv-update && \
        init_pyenv
fi

# Go
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/shims:$PATH"
export PATH="$GOENV_ROOT/bin:$PATH"
if [[ -d $GOENV_ROOT ]]; then
    init_goenv
else
    git clone git@github.com:wfarr/goenv.git $GOENV_ROOT && \
        git clone git://github.com/juxtaposedwords/goenv-update.git $GOENV_ROOT/plugins/goenv-update && \
        init_goenv
fi

export GOPATH="$HOME/workspace/go-projects"
export GOROOT="/usr/local/go"
export PATH="$GOPATH/bin:$PATH"
if [[ ! -d $GOPATH ]]; then
    mkdir -p $GOPATH
fi

# Node
export NODENV_ROOT="$HOME/.nodenv"
export PATH="$NODENV_ROOT/shims:$PATH"
export PATH="$NODENV_ROOT/bin:$PATH"
if [[ -d $NODENV_ROOT ]]; then
    init_nodenv
else
    git clone https://github.com/nodenv/nodenv.git $NODENV_ROOT && \
        git clone https://github.com/nodenv/node-build.git $NODENV_ROOT/plugins/node-build && \
        git clone https://github.com/nodenv/nodenv-update.git $NODENV_ROOT/plugins/nodenv-update && \
        cd ~/.nodenv && src/configure && make -C src && cd
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
export MYVIMRC='~/.vimrc'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/"

# ================================================================================
#   Alias
# ================================================================================

if ! type hub > /dev/null ; then
    mkdir -p ~/tmp/hub && git clone git@github.com:github/hub.git ~/tmp/hub/hub
    cd ~/tmp/hub/hub && ./script/build -o ~/bin/hub
fi

alias zshconfig="$EDITOR $CONFIG/.zshrc"
alias gitconfig="$EDITOR $CONFIG/.gitconfig"
alias C='clear'
alias be='bundle exec'
alias git='hub'
alias sbcl='rlwrap sbcl'
alias ccl='rlwrap ccl64'
alias emacs='emacs -nw'
alias clean_git_branches='git gc --prune=now && git remote prune origin'
alias gst='git status'
alias py='python'
alias ta='tmux attach -t'
alias tad='tmux attach -d -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'
alias tksv='tmux kill-server'
alias tkss='tmux kill-session -t'

if type nvim > /dev/null ; then
    alias vim='nvim'
fi

# ================================================================================
#   Functions
# ================================================================================

function weather() {
    local LOCATION="SanFrancisco"
    if [ $1 ]; then
	LOCATION="$1"
    fi
    eval 'curl -sS wttr.in/'$LOCATION
}

# ================================================================================
#   Powerline Configuration
# ================================================================================

if [[ "$OS" == "Linux" ]]; then
    export PATH="$HOME/.local/bin:$PATH"
    export TERM="xterm-256color"
fi

export PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'
