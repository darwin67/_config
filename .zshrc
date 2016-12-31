#!/usr/local/bin/zsh
#
# ===
#   Zsh configs
#

# Path to configuration directory
export CONFIG="$HOME/_config"

source $CONFIG/zsh_conf/_shell
source $CONFIG/zsh_conf/_path

# The current OS
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export OS="Linux"
    source $CONFIG/zsh_conf/_linux
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export OS="Mac"
    source $CONFIG/zsh_conf/_osx
fi

source $CONFIG/zsh_conf/_version_control
source $CONFIG/zsh_conf/_env
source $CONFIG/zsh_conf/_alias
source $CONFIG/zsh_conf/_functions
source $CONFIG/zsh_conf/_completions
source $CONFIG/zsh_conf/_tmux


# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle :compinstall filename '/home/darwin/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
