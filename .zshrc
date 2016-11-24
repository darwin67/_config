#!/usr/local/bin/zsh
#
# ===
#   Zsh configs
#

# Path to configuration directory
export CONFIG="$HOME/_config"

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

source $CONFIG/zsh_conf/_path

# The current OS
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export OS="Linux"
    source $CONFIG/zsh_conf/_linux
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export OS="Mac"
    source $CONFIG/zsh_conf/_osx
fi

source $CONFIG/zsh_conf/_git
source $CONFIG/zsh_conf/_version_control
source $CONFIG/zsh_conf/_env
source $CONFIG/zsh_conf/_alias
source $CONFIG/zsh_conf/_functions
source $CONFIG/zsh_conf/_tmux
