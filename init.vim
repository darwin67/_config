let s:cache_home    = empty($XDG_CACHE_HOME) ? expand('~/.cache') : $XDG_CACHE_HOME
let s:dein_dir      = s:cache_home . '/dein'
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
" let g:python_host_prog = $HOME . '/.pyenv/shims/python2'
let g:python3_host_prog = $HOME . '/.pyenv/shims/python3'

if !isdirectory(s:dein_repo_dir)
   call system('git clone https://github.com/Shougo/dein.vim ' . shellescape(s:dein_repo_dir))
endif

let &runtimepath = s:dein_repo_dir . "," . &runtimepath

if &compatible
  set nocompatible
endif

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  call dein#add(s:dein_repo_dir)
  " call dein#add('plasticboy/vim-markdown')
  call dein#add('thinca/vim-quickrun')

  call dein#add('Shougo/deoplete.nvim')
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif

  call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('airblade/vim-gitgutter')
  call dein#add('scrooloose/nerdtree')
  call dein#add('Xuyuanp/nerdtree-git-plugin')
  call dein#add('tpope/vim-fugitive')
  call dein#add('Townk/vim-autoclose')
  " call dein#add('scrooloose/syntastic')
  call dein#add('tomtom/tcomment_vim')
  call dein#add('szw/vim-tags')
  " call dein#add('leafgarland/typescript-vim')

  " Load on command
  " -------------------------------------------------------------
  call dein#add('majutsushi/tagbar', { 'on_cmd' : [ 'TagbarToggle' ] })
  " call dein#add('mileszs/ack.vim', { 'on_cmd' : [ 'Ack' ] })

  " Auto complete
  " -------------------------------------------------------------
  call dein#add('zchee/deoplete-clang')
  call dein#add('sebastianmarkow/deoplete-rust')

  " Themes
  call dein#add('gosukiwi/vim-atom-dark')
  call dein#add('sheerun/vim-polyglot')

  call dein#end()
  call dein#save_state()
endif

" auto install plugins
if has('vim_starting') && dein#check_install()
  call dein#install()
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors, Fonts and Theme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_powerline_fonts = 1
let g:airline_theme='onedark'

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

syntax enable
colorscheme atom-dark

" let g:make = 'gmake'
" if system('uname -o') =~ '^GNU/'
"   let g:make = 'make'
" endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Delete trailing white space on save
function StripTrailingWhitespace()
  " Strip only if the b:no_strip_whitespace is enabled
  if exists('b:no_strip_whitespace')
    return
  endif
  %s/\s\+$//ge
endfunction

" Toggle line number
function ToggleLineNumber()
  if &number
    set nonumber
  else
    set number
 endif
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Autocompletion (Deoplete)
"
" Requires python 3
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable autocomplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_ignore_case = 1
let g:deoplete#enable_smart_case = 1
inoremap <expr> <Tab> pumvisible()? "\<C-n>" : "\<Tab>"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets Leader to ','
let mapleader = ","

" Sets how many lines of history VIM has to remember
set history=700

" Enable filetype plugins and indent
filetype plugin indent on

" Set to auto read when a file is changed from the outside
set autoread



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu

"Always show current position
set ruler

" Height of the command bar
set cmdheight=1

" A buffer becomes hidden when it is abandoned
set hidden

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Maximum the help screens
set helpheight=999

" Confirm before closing file
set confirm

" See at least 8 lines when scrolling up or down
set scrolloff=8

" Prevent extra indentation when copy and pasting
" Note: not compatible with neocomplete
" set paste

" Remap copy and paste to clipboard instead of buffer
set clipboard=unnamed

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

" Line number
nnoremap <silent>ln :call ToggleLineNumber()<CR>



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" Reloads .vimrc
nnoremap <leader>so :so $MYVIMRC<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Be smart when using tabs
set smarttab

" 1 tab = 2 spaces
set shiftwidth=2 softtabstop=2 tabstop=2 expandtab

" Linebreak on 500 characters
set lbr
set tw=500

set autoindent                  " Auto indent
set smartindent                 " Smart indent
set wrap                        " Wrap lines
set wrapscan

set t_Co=256                    " use 256 colors for screen
set termguicolors               " use True Color

set list                        " make space visible
set listchars=tab:»\ ,trail:-,extends:»,precedes:«,nbsp:% " space

set cursorline
" set cursorcolumn


""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""
" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Window spliting
nnoremap <leader>- :split<CR>
nnoremap <leader>\| :vsplit<CR>

" Close the current buffer
nnoremap <leader>bd :Bclose<cr>

" Close all the buffers
nnoremap <leader>ba :1,1000 bd!<cr>

" Useful mappings for managing tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
" nnoremap <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/
nnoremap <leader>te :tabedit

" Switch CWD to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry
" Remember info about open buffers on close
set viminfo^=%

" Return to last edit position when opening files
augroup last_edit
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END


""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" Format the status line
" set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap VIM 0 to first non-blank character
noremap 0 ^

" Move a line of text using ALT+[jk]
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ack searching, requires ack.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" When you press gv you Ack after the selected text
vnoremap <silent>gv :call VisualSelection('gv', '')<CR>

" Open Ack and put the cursor in the right position
nnoremap <leader>a :Ack
" When you press <leader>r you can search and replace the selected text
vnoremap <silent><leader>r :call VisualSelection('replace', '')<CR>



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pressing ,ss will toggle and untoggle spell checking
nnoremap <leader>ss :setlocal spell!<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerdtree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Mappings
nnoremap <leader>nt :NERDTree<CR>
nnoremap <leader>nm :NERDTreeMirror<CR>
nnoremap <leader>nc :NERDTreeClose<CR>

" Bookmark location
let g:NERDTreeBookmarksFile = expand(s:dein_dir . '/NERDTree/.NERDTreeBookmarks')

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>gn :Git 
nnoremap <leader>ga :Git add .<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gsd :Gsdiff<CR>
nnoremap <leader>gvd :Gvdiff<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gc :Gcommit -v<CR>
nnoremap <leader>gh :Git stash<CR>
nnoremap <leader>gu :Git stash pop<CR>
nnoremap <leader>gp :Git push 
