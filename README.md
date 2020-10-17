# Config files and dotfiles

Settings that are shared across computers.

## Prerequisites

**Ubuntu**
```bash
# Ruby
$ sudo apt install -y autoconf bison build-essential libssl-dev libyaml-dev libreadline6-dev zlib1g-dev libncurses5-dev libffi-dev libgdbm5 libgdbm-dev texinfo

# Python
$ sudo apt install -y make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev
```

**Arch**
```bash
# Ruby
$ sudo pacman -S --needed base-devel libffi libyaml openssl zlib

# Python
$ sudo pacman -S base-devel openssl zlib xz

# Fonts
$ sudo pacman -S noto-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-extra adobe-source-han-sans-jp-fonts
```

## Powerline fonts for both Vim and Tmux

```bash
$ git clone git@github.com:powerline/fonts.git
$ fonts/install.sh
```

Set font on iTerm or Terminal
```
Meslo LG S for Powerline
```

## Additional addons

```
# copyq
$ sudo add-apt-repository ppa:hluk/copyq

# Clang
# source: http://apt.llvm.org/
```

## Post language installation

Run the following script to install the common libraries used.
```bash
$ setup
```

#### Elixir

``` bash
$ export MIX_ENV=prod
$ git clone git@github.com:elixir-lsp/elixir-ls.git
$ mix deps.get
$ mix compile
$ mix elixir_ls.release -o ~/.lsp/elixir-ls
```

