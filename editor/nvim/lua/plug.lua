--
-- Set up Plug for plugin management
--
-- install with the following command
-- curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

local Plug = vim.fn['plug#']

-- Plugins will be downloaded under this directory
vim.call('plug#begin', '~/.vim/plugged')

Plug 'jnurmine/zenburn'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdtree'

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'imsnif/kdl.vim'

-- Finished
vim.call('plug#end')
