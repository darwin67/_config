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
" Colors, Fonts and Theme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colors zenburn
let g:airline_powerline_fonts = 1
let g:airline_theme='onedark'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Core configurations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let mapleader = ',' " key leader
syntax enable
filetype plugin indent on  " enable filetype plugins and indent

" disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

set encoding=utf8                                              " set utf8 as standard encoding
set history=1000                                               " the amount of history to preserve
set autoread                                                   " set to autoread when a file is changed outside of Vim
set wildmenu                                                   " turn on wild menu
set ruler                                                      " always show the current position
set cmdheight=1                                                " height of the command bar
set hidden                                                     " a buffer becomes hidden when it's abandoned
set ignorecase                                                 " ignore case when searching
set smartcase                                                  " but also try to be smart when searching
set hlsearch                                                   " highlight search results
set incsearch                                                  " makes search act like search in modern browsers
set lazyredraw                                                 " don't redraw while executing macros (good performance config)
set magic                                                      " for regular expressions, turn magic on
set showmatch                                                  " show matching brackets under the cursor
set mat=2                                                      " how many tenths of a second to blink when matching brackets
set helpheight=999                                             " maximize help screen
set confirm                                                    " confirm before closing file
set scrolloff=8                                                " see at least 8 lines when scroliing up or down
set paste                                                      " prevent extra indentation when copy and pasting
set clipboard=unnamed                                          " remap copy and paste to clipboard instead of buffer
set smarttab                                                   " be smart when using tabs
set shiftwidth=2 softtabstop=2 tabstop=2 expandtab             " 1 tab = 2 spaces
set nobackup nowb noswapfile                                   " turn backup off since most stuff are in version control anyway
set backspace=eol,start,indent whichwrap+=<,>,h,l              " configure backspace to act like one
set noerrorbells novisualbell t_vb= tm=500                     " disable annoying sounds on errors
set lbr tw=500                                                 " line break on 500 characters
set autoindent smartindent                                     " nice indentation
set wrap wrapscan                                              " wrap lines
set list listchars=tab:»\ ,trail:-,extends:»,precedes:«,nbsp:% " make space visible
set viminfo^=%                                                 " remember info about open buffers on close
set laststatus=2                                               " always show the status line

set cursorline " highlight current cusor's location
hi CursorLine term=bold cterm=none ctermbg=8 guibg=Grey40

" enable mouse if it has one
if has('mouse')
  set mouse=a
endif

" Key Mappings
" ==================

" Copy
nnoremap y "*y
nnoremap yy "*yy
vnoremap Y "*Y
vnoremap y "*y

" Cut
nnoremap dd "+dd
nnoremap D "+D
vnoremap D "+D
vnoremap X "+X
vnoremap d "+d
vnoremap x "+x

" Paste
nnoremap p "*p
nnoremap P "*P
nnoremap gp "*gp
nnoremap gP "*gP

" reloads Vim
" nnoremap <leader>so :so $MYVIMRC<cr>

" window spliting
nnoremap <leader>- :split<cr>
nnoremap <leader>\| :vsplit<cr>
autocmd VimResized * wincmd =

" tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclear<cr>
nnoremap <leader>tm :tabmove

" opens a new tab with the current buffer's path
" super useful when editing files in the same directory
" nnoremap <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/
nnoremap <leader>te :tabedit

" switch cwd to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>

" specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

" return to last edit position when opening files
augroup last_edit
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END

" Move a line of text using ALT+[jk]
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" When you press gv you Ack after the selected text
vnoremap <silent>gv :call VisualSelection('gv', '')<cr>

" Open Ack and put the cursor in the right position
nnoremap <leader>a :Ack
" When you press <leader>r you can search and replace the selected text
vnoremap <silent><leader>r :call VisualSelection('replace', '')<cr>

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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin Configurations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" git
nnoremap <leader>gn :Git-
nnoremap <leader>ga :Git add .<cr>
nnoremap <leader>gs :Git status<cr>
nnoremap <leader>gsd :Gsdiff<cr>
nnoremap <leader>gvd :Gvdiff<cr>
nnoremap <leader>gb :Git blame<cr>
nnoremap <leader>gc :Git commit -v<cr>
nnoremap <leader>gh :Git stash<cr>
nnoremap <leader>gu :Git stash pop<cr>
nnoremap <leader>gp :Git push


" nerdtree
nnoremap <leader>nb :NERDTree<cr>
nnoremap <leader>nm :NERDTreeMirror<cr>
nnoremap <leader>nc :NERDTreeClose<cr>

" Nerdtree git plugin
let g:NERDTreeIndicatorMapCustom = {
  \ "Modified"  : "✹",
  \ "Staged"    : "✚",
  \ "Untracked" : "✭",
  \ "Renamed"   : "➜",
  \ "Unmerged"  : "═",
  \ "Deleted"   : "✖",
  \ "Dirty"     : "✗",
  \ "Clean"     : "✔︎",
  \ "Unknown"   : "?"
  \ }

let g:NERDTreeIgnore = [ '\.sock$', '\.git$' ]

augroup nerdtree_group
  autocmd!
  autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
augroup END

