#!/bin/zsh
#
# Custom ZSH Theme based on
# https://github.com/denysdovhan/spaceship-zsh-theme
#

NEWLINE='
'

# PROMPT
PROMPT_SYMBOL="${PROMPT_SYMBOL:->>}"
PROMPT_ADD_NEWLINE="${PROMPT_ADD_NEWLINE:-true}"
PROMPT_SEPARATE_LINE="${PROMPT_SEPARATE_LINE:-true}"
PROMPT_TRUNC="${PROMPT_TRUNC:-3}"

# GIT
GIT_SHOW="${GIT_SHOW:-true}"
GIT_UNCOMMITTED="${GIT_UNCOMMITTED:-+}"
GIT_UNSTAGED="${GIT_UNSTAGED:-✹}"
GIT_UNTRACKED="${GIT_UNTRACKED:-?}"
GIT_STASHED="${GIT_STASHED:-$}"
GIT_UNPULLED="${GIT_UNPULLED:-⇣}"
GIT_UNPUSHED="${GIT_UNPUSHED:-⇡}"

# NODE
NODE_SHOW="${NODE_SHOW:-true}"

# RUBY
RUBY_SHOW="${RUBY_SHOW:-true}"

# PYTHON
PYTHON_SHOW="${PYTHON_SHOW:-true}"

# Current directory.
# Return only three last items of path
current_dir() {
    echo -n "%{$fg_bold[cyan]%}"
    echo -n "%${PROMPT_TRUNC}~";
    echo -n "%{$reset_color%}"
}

# Uncommitted changes.
# Check for uncommitted changes in the index.
git_uncomitted() {
    if ! $(git diff --quiet --ignore-submodules --cached); then
	echo -n "%{$fg_bold[green]%}"
	echo -n "${GIT_UNCOMMITTED}"
	echo -n "%{$reset_color%}"
    fi
}

# Unstaged changes.
# Check for unstaged changes.
git_unstaged() {
    if ! $(git diff-files --quiet --ignore-submodules --); then
	echo -n "%{$fg_bold[red]%}"
	echo -n "${GIT_UNSTAGED}"
	echo -n "%{$reset_color%}"
    fi
}

# Untracked files.
# Check for untracked files.
git_untracked() {
    if [ -n "$(git ls-files --others --exclude-standard)" ]; then
	echo -n "%{$fg_bold[magenta]%}"
	echo -n "${GIT_UNTRACKED}"
	echo -n "%{$reset_color%}"
    fi
}

# Stashed changes.
# Check for stashed changes.
git_stashed() {
    if $(git rev-parse --verify refs/stash &>/dev/null); then
	echo -n "%{$fg_bold[yellow]%}"
	echo -n "${GIT_STASHED}"
	echo -n "%{$reset_color%}"
    fi
}

# Unpushed and unpulled commits.
# Get unpushed and unpulled commits from remote and draw arrows.
git_unpushed_unpulled() {
    # check if there is an upstream configured for this branch
    command git rev-parse --abbrev-ref @'{u}' &>/dev/null || return

    local count
    count="$(command git rev-list --left-right --count HEAD...@'{u}' 2>/dev/null)"
    # exit if the command failed
    (( !$? )) || return

    # counters are tab-separated, split on tab and store as array
    count=(${(ps:\t:)count})
    local arrows left=${count[1]} right=${count[2]}

    (( ${right:-0} > 0 )) && arrows+="${GIT_UNPULLED}"
    (( ${left:-0} > 0 )) && arrows+="${GIT_UNPUSHED}"

    [ -n $arrows ] && echo -n "${arrows}"
}

# Git status.
# Collect indicators, git branch and pring string.
git_status() {
    [[ $GIT_SHOW == false ]] && return

    # Check if the current directory is in a Git repository.
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    # Check if the current directory is in .git before running git checks.
    if [[ "$(git rev-parse --is-inside-git-dir 2> /dev/null)" == 'false' ]]; then
	# Ensure the index is up to date.
	git update-index --really-refresh -q &>/dev/null

	# String of indicators
	local indicators=''

	indicators+="$(git_uncomitted)"
	indicators+="$(git_unstaged)"
	indicators+="$(git_untracked)"
	indicators+="$(git_stashed)"
	indicators+="$(git_unpushed_unpulled)"

	[ -n "${indicators}" ] && indicators=" {${indicators}}";

	echo -n " %Bon%b "
	echo -n "%{$fg_bold[magenta]%}"
	echo -n "$(git_current_branch)"
	echo -n "%{$reset_color%}"
	echo -n "%{$indicators%}"
    fi
}

# Virtual environment.
# Show current virtual environment (Python).
python_version() {
    [[ $PYTHON_SHOW == false ]] && return

    $(command -v pyenv > /dev/null 2>&1) || return

    python_version=$(pyenv version | sed -e 's/ (set.*$//')

    # Check if the current directory running via Virtualenv
    [ -n "${python_version}" ] || return
    echo -n "%{$fg_bold[blue]%}"
    echo -n "${python_version}"
    echo -n "%{$reset_color%}"
}

# NODE
# Show current version of node, exception system.
node_version() {
    [[ $NODE_SHOW == false ]] && return

    $(type nodenv >/dev/null 2>&1) || return

    node_version=$(nodenv version | sed -e 's/ (set.*$//')

    echo -n "%{$fg_bold[green]%}"
    echo -n "${node_version}"
    echo -n "%{$reset_color%}"
}

# Ruby
# Show current version of Ruby
ruby_version() {
    [[ $RUBY_SHOW == false ]] && return

    $(type rbenv > /dev/null 2>&1) || return

    ruby_version=$(rbenv version | sed -e 's/ (set.*$//')

    echo -n "%{$fg_bold[red]%}"
    echo -n "${ruby_version}"
    echo -n "%{$reset_color%}"
}

# Command prompt.
# Pain $PROMPT_SYMBOL in red if previous command was fail and
# pain in green if all OK.
return_status() {
    echo -n "%(?.%{$fg[green]%}.%{$fg[red]%})"
    echo -n "%B${PROMPT_SYMBOL}%b"
    echo    "%{$reset_color%}"
}

# Build prompt line
build_prompt() {
    current_dir
    git_status
    echo " ( $(node_version) $(ruby_version) $(python_version) )"
}

# Compose PROMPT
PROMPT=''
[[ $PROMPT_ADD_NEWLINE == true ]] && PROMPT="$PROMPT$NEWLINE"
PROMPT="$PROMPT"'$(build_prompt) '
[[ $PROMPT_SEPARATE_LINE == true ]] && PROMPT="$PROMPT$NEWLINE"
PROMPT="$PROMPT"'$(return_status) '

# Set PS2 - continuation interactive prompt
PS2="%{$fg_bold[yellow]%}"
PS2+="%{$PROMPT_SYMBOL%} "
PS2+="%{$reset_color%}"

# LSCOLORS
export LSCOLORS="Gxfxcxdxbxegedabagacab"
export LS_COLORS='no=00:fi=00:di=01;34:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;32:ow=0;41:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.avi=01;35:*.bmp=01;35:*.fli=01;35:*.gif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mng=01;35:*.mov=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.tga=01;35:*.tif=01;35:*.xbm=01;35:*.xpm=01;35:*.dl=01;35:*.gl=01;35:*.wmv=01;35:*.aiff=00;32:*.au=00;32:*.mid=00;32:*.mp3=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:*.patch=00;34:*.o=00;32:*.so=01;35:*.ko=01;31:*.la=00;33'
# Zsh to use the same colors as ls
# Link: http://superuser.com/a/707567
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
