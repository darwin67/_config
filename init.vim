" install with the following command
" curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

" Plugins will be downloaded under this directory
call plug#begin('~/.vim/plugged')

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

" Plugin list end
call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Core configurations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Configs with Lua
lua require('core')
lua require('core-keymap')
lua require('plugin-git')
  lua require('plugin-nerdtree')

" Key Mappings
" ==================

" delete trailing whitespace on save
function StripTrailingWhitespace()
  " disable strip if the b:no_strip_whitespace is enabled
  if exists('b:no_strip_whitespace')
    return
  endif
  %s/\s\+$//ge
endfunction
autocmd BufWritePre * call StripTrailingWhitespace()

" toggle line number
function ToggleLineNumber()
  if &number
    set nonumber
  else
    set number
  endif
endfunction
nnoremap <leader>ln :call ToggleLineNumber()<cr>

