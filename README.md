# Config files and dotfiles

Settings that are shared across computers.

## Prerequisites

**Arch**
```bash
# Ruby
$ sudo pacman -S --needed base-devel libffi libyaml openssl zlib

# Python
$ sudo pacman -S base-devel openssl zlib xz

# Erlang
# Reference: https://github.com/asdf-vm/asdf-erlang#arch-linux
$ sudo pacman -S ncurses glu mesa wxgtk2 libpng libssh unixodbc libxslt fop

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
$ mix elixir_ls.release -o ~/.lsp/elixir
```

Add `~/.lsp/elixir` to `$PATH` and run `doom env` to make sure Doom know about that path.

