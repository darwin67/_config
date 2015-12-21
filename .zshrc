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
plugins=(git github rails nvm tmux gnu-utils history)

# The current OS
if [ "$DESKTOP_SESSION" = "ubuntu" ]; then
    export OS="Linux"
else
    export OS="Mac"
fi

# User configuration
# Paths
export PATH="/usr/local/git/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/sbin:$PATH"
export PATH="/usr/bin:$PATH"
export PATH="/sbin:$PATH"
export PATH="/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/.rbenv/bin:$PATH"

if [ $OS = "Mac" ]; then
    export PATH="/usr/local/bin/ctags:$PATH"
    export PATH="/opt/local/sbin/usr/bin:$PATH"
    export PATH="/opt/local/bin:$PATH"
    export PATH="/opt/chefdk/bin:$PATH"
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
elif [ $OS = "Linux" ]; then
    export PATH="/usr/games:$PATH"
    export PATH="/usr/local/games:$PATH"
fi

export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# Init rbenv
eval "$(rbenv init -)"

# You may need to manually set your language environment
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='vim'
export MYVIMRC='~/.vimrc'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/"

# Alias
alias zshconfig="$EDITOR $CONFIG/.zshrc"
alias gitconfig="$EDITOR $CONFIG/.gitconfig"
alias C="clear"
alias be="bundle exec"
alias git=hub
alias gloga="git log --graph --simplify-by-decoration --all"

# Golang setups
export PATH="/usr/local/go/bin:$PATH"
mkdir -p $HOME/go_projects
export GOROOT="/usr/local/go"
export GOPATH="$HOME/go_projects"
export PATH="$GOPATH/bin:$PATH"

# Powerline config
if [ "$OS" = "Mac" ]; then
    export PATH="$PATH:$HOME/Library/Python/2.7/bin"
elif [ "$OS" = "Linux" ]; then
    export PATH="$PATH:$HOME/.local/bin"
    export TERM="xterm-256color"
fi

powerline-daemon -q
export PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

