# Script Install
```sh
./install [config]
```
config:
- brew       - install brew (only for Mac OS)
- vim        - install vim with lua
- zsh        - install Zsh
- powerline  - install Powerline
- tmux       - install Tmux
- link       - link dotfiles to $HOME

# Manual Install
## Install Zsh
```
brew install zsh
echo '/usr/local/bin/zsh' >> /etc/shells # Add to /etc/shells directly if not successful
sudo chsh -s /usr/local/bin/zsh
```

### Mac
*Normal Vim*
```
brew install vim --with-lua
```

### Linux (Ubuntu)
- [Building vim on Ubuntu](http://vim.wikia.com/wiki/Building_Vim)
- [vim 7.4 with lua+GUI on Ubuntu 14.04](https://gist.github.com/darwin67/44668fad5c94a9946cba)

```sh
#!/bin/sh

sudo apt-get remove --purge vim vim-runtime vim-gnome vim-tiny vim-common vim-gui-common
sudo apt-get install liblua5.1-dev luajit libluajit-5.1 python-dev ruby-dev libperl-dev mercurial libncurses5-dev libgnome2-dev libgnomeui-dev libgtk2.0-dev libatk1.0-dev libbonoboui2-dev libcairo2-dev libx11-dev libxpm-dev libxt-dev

sudo mkdir -p /usr/include/lua5.1/include
sudo ln -sf /usr/include/luajit-2.0 /usr/include/lua5.1/include

cd ~
hg clone https://code.google.com/p/vim/
cd vim/src
make distclean
./configure --with-features=huge \
            --enable-rubyinterp \
            --enable-largefile \
            --disable-netbeans \
            --enable-pythoninterp \
            --with-python-config-dir=/usr/lib/python2.7/config-x86_64-linux-gnu \
            --enable-perlinterp \
            --enable-luainterp \
            --with-luajit \
            --enable-gui=auto \
            --enable-fail-if-missing \
            --with-lua-prefix=/usr/include/lua5.1 \
            --enable-cscope
make

sudo make install

cd ..
sudo mkdir -p /usr/share/vim
sudo mkdir -p /usr/share/vim/vim74
sudo cp -fr runtime/* /usr/share/vim/vim74/
```

## Setup Powerline Mac version (for both vim and tmux)
```
brew install python
pip install --user powerline-status
mkdir -p ~/tmp && cd ~/tmp || exit
git clone git@github.com:powerline/fonts.git
fonts/install.sh
```

Set font on iTerm or Terminal
```
Meslo LG S for Powerline
```

Add the following to zsh (or bash)
```
echo "PATH=\"$PATH:$HOME/Library/Python/2.7/bin\"" >> ~/.zshrc # Mac
echo "PATH=\"$PATH:$HOME/.local/bin\" >> ~/.zshrc              # Ubuntu

echo "powerline-daemon -q" >> ~/.zshrc
```

### Searchers
- [highway](https://github.com/tkengo/highway)
- [the sliver searcher (ag)](https://github.com/ggreer/the_silver_searcher)
- [the platinum searcher (pt)](https://github.com/monochromegane/the_platinum_searcher)
- [ack](http://beyondgrep.com/)

### External syntax checkers
- [GCC](https://github.com/scrooloose/syntastic/wiki/C--%3A---gcc) - C, C++
- [CppCheck](http://cppcheck.sourceforge.net/) - C++
- [Rubocop](https://github.com/bbatsov/rubocop) - Ruby
- [JSHint](https://github.com/eslint/eslint) - Javascript
- [ESLint](https://github.com/eslint/eslint) - Javascript
- [JSONLint](https://www.npmjs.com/package/jsonlint) - JSON
- [Recess](http://twitter.github.io/recess/) - CSS, LESS
- [scss-lint](https://github.com/brigade/scss-lint) - SCSS, SASS
- [slim-lint](https://github.com/sds/slim-lint) - Slim
- [dockerfile-lint](https://github.com/projectatomic/dockerfile_lint) - Docker
- [sqlint](https://github.com/purcell/sqlint) - SQL
- [golint](https://github.com/golang/lint) - Golng (installed with vim-go)
- [govet](https://golang.org/cmd/vet/) - Golng (installed with vim-go)
- [errcheck](https://github.com/kisielk/errcheck) - Golang (installed with vim-go)
- [markdownlint](https://github.com/mivok/markdownlint) - Markdown
- [shellcheck](https://github.com/koalaman/shellcheck) - Shell script
