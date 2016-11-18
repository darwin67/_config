#!/bin/zsh
#
# Custom ZSH Theme based on
# https://github.com/denysdovhan/spaceship-zsh-theme
#

pmodload 'helper'

NEWLINE='
'

# PROMPT
PROMPT_SYMBOL="${PROMPT_SYMBOL:->>}"
PROMPT_TRUNC="${PROMPT_TRUNC:-3}"

# NODE
# Show current version of node, exception system.
function _set_node_version {
    $(type nodenv >/dev/null 2>&1) || return
    local version=$(nodenv version | sed -e 's/ (set.*$//')

    _node_version="%F{green}%B${version}%b%f"
}

# Ruby
# Show current version of Ruby
function _set_ruby_version {
    $(type rbenv > /dev/null 2>&1) || return
    local version=$(rbenv version | sed -e 's/ (set.*$//')

    _ruby_version="%F{red}%B${version}%b%f"
}

# Python
# Show current version of Python
function _set_python_version {
    $(type pyenv > /dev/null 2>&1) || return
    local version=$(pyenv version | sed -e 's/ (set.*$//')

    _python_version="%F{blue}%B${version}%b%f"
}

function prompt_custom_precmd {
    setopt LOCAL_OPTIONS
    unsetopt XTRACE KSH_ARRAYS

    if (( $+functions[git-info] )); then
        git-info
    fi

    _current_dir="%F{cyan}%B%${PROMPT_TRUNC}~%b%f" # set current directory
    # Paint $PROMPT_SYMBOL in red if previous command failed and green if all OK
    _status="%(?.%F{green}.%F{red})%B${PROMPT_SYMBOL}%b%f"
    _set_node_version
    _set_ruby_version
    _set_python_version
}

function prompt_custom_setup {
    setopt LOCAL_OPTIONS
    unsetopt XTRACE KSH_ARRAYS
    prompt_opts=(cr percent subst)

    # Load required functions
    autoload -Uz add-zsh-hook

    add-zsh-hook precmd prompt_custom_precmd

    # set python-info
    zstyle ':prezto:module:python:info:virtualenv' format '%B%F{blue}[%v]%f%b'

    # set git-info params
    zstyle ':prezto:module:git:info' verbose 'yes'
    zstyle ':prezto:module:git:info:action' format '%F{white}:%f%%B%F{bright red}%s%f%%b'
    zstyle ':prezto:module:git:info:added' format ' %%B%F{green}✚%f%%b'
    zstyle ':prezto:module:git:info:ahead' format ' %%B%F{cyan}⬆%f%%b'
    zstyle ':prezto:module:git:info:behind' format ' %%B%F{red}⬇%f%%b'
    zstyle ':prezto:module:git:info:commit' format ' %%B%F{magenta}%.7c%f%%b'
    zstyle ':prezto:module:git:info:deleted' format ' %%B%F{red}✖%f%%b'
    zstyle ':prezto:module:git:info:modified' format ' %%B%F{white}✱%f%%b'
    zstyle ':prezto:module:git:info:position' format ' %%B%F{magenta}%p%f%%b'
    zstyle ':prezto:module:git:info:renamed' format ' %%B%F{yellow}➜%f%%b'
    zstyle ':prezto:module:git:info:stashed' format ' %%B%F{yellow}$%f%%b'
    zstyle ':prezto:module:git:info:unmerged' format ' %%B%F{yellow}═%f%%b'
    zstyle ':prezto:module:git:info:untracked' format ' %%B%F{magenta}?%f%%b'
    zstyle ':prezto:module:git:info:branch' format '%%B%F{magenta}%b%f%%b'
    zstyle ':prezto:module:git:info:keys' format \
           'prompt' '$(coalesce "%b" "%p" "%c")%s%A%B%S%a%d%m%r%U%u' \
           'rprompt' ''

    PROMPT='$NEWLINE${_current_dir} < ${_node_version} ${_ruby_version} ${_python_version} > ${git_info:+${(e)git_info[prompt]}} $NEWLINE${_status} '
    # RPROMPT=''

    # PS2='%F{yellow}${PROMPT_SYMBOL}%f'
}

prompt_custom_setup "$@"
