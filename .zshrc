# ================================================================================
#   Basic Configutations
# ================================================================================

# Path to configuration directory
export CONFIG="$HOME/_config"

# Path to your oh-my-zsh installation.
export ZSH=$CONFIG/modules/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="amuse"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git github gem rails pip nvm tmux gnu-utils history aws docker)

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

source $ZSH/oh-my-zsh.sh

# Init rbenv
export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/shims:$PATH"
export PATH="$RBENV_ROOT/bin:$PATH"
if [ -d "$RBENV_ROOT" ]; then
    eval "$(rbenv init -)"
else
    git clone https://github.com/rbenv/rbenv.git ~/.rbenv
    git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
export PATH="$PYENV_ROOT/bin:$PATH"
if [ -d "$PYENV_ROOT" ]; then
    eval "$(pyenv init -)"
else
    git clone https://github.com/yyuu/pyenv.git ~/.pyenv
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

alias zshconfig="$EDITOR $CONFIG/.zshrc"
alias gitconfig="$EDITOR $CONFIG/.gitconfig"
alias C="clear"
alias be="bundle exec"
alias git=hub
alias sbcl='rlwrap sbcl'
alias ccl='rlwrap ccl64'
alias emacs='emacs -nw'

# ================================================================================
#   Specific Settings
# ================================================================================

# Golang setups
export PATH="/usr/local/go/bin:$PATH"
mkdir -p $HOME/go_projects
export GOROOT="/usr/local/go"
export GOPATH="$HOME/go_projects"
export PATH="$GOPATH/bin:$PATH"

# ================================================================================
#   Functions
# ================================================================================

# generate tags for javascript files
function generate_jsctags() {
    if type jsctags > /dev/null; then
        find . -type f -iregex .*\.js$ -not -path "./node_modules/*" -exec jsctags {} -f \; | sed '/^$/d' | sort > tags
        # ctags .
    else
        echo "jsctags is not installed."
        echo "please go to https://github.com/ramitos/jsctags and install from there."
    fi
}

# Get the weather
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
