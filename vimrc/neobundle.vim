"NeoBundle Scripts-----------------------------
if has('vim_starting')
  if &compatible
    set nocompatible
  endif

  " Required:
  set runtimepath+=$CONFIG/bundle/neobundle.vim/
endif

let g:neobundle#log_filename = $CONFIG . '/tmp/neobundle.log'
let g:junkfile#directory=expand($CONFIG . "/tmp/junk")

" Required:
call neobundle#begin(expand('$CONFIG/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Always load these
" -------------------------------------------------------------
NeoBundle 'Shougo/vimproc.vim', {
	\ 'build' : {
	\     'windows' : 'tools\\update-dll-mingw',
	\     'cygwin' : 'make -f make_cygwin.mak',
	\     'mac' : 'make -f make_mac.mak',
	\     'linux' : 'make',
	\     'unix' : 'gmake',
	\    },
	\ }
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'Xuyuanp/nerdtree-git-plugin'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'Townk/vim-autoclose'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'szw/vim-tags'
NeoBundle 'Shougo/unite.vim', { 'depends': [ 'Shougo/neomru.vim' ] }


" Lazy loading
" -------------------------------------------------------------
NeoBundleLazy 'rizzatti/dash.vim', {
            \ 'autoload': { 'commands': 'Dash' },
            \ }
NeoBundleLazy 'majutsushi/tagbar', {
            \ 'autoload': { 'commands': [ 'TagbarToggle' ] },
            \ }
NeoBundleLazy 'mileszs/ack.vim', {
            \ 'autoload': { 'commands': [ 'Ack' ] },
            \ }
NeoBundleLazy 'tyru/open-browser-github.vim', {
            \ 'depends': [ 'tyru/open-browser.vim' ],
            \ 'autoload': { 'commands': [ 'OpenGithubFile', 'OpenGithubIssue', 'OpenGithubPullReq' ] }
            \ }

" Syntax related stuff
NeoBundleLazy 'vim-jp/vim-cpp', { 'filetypes': [ 'c', 'cpp' ] }
NeoBundleLazy 'tpope/vim-rails', { 'filetypes': 'ruby' }
NeoBundleLazy 'moll/vim-node', { 'filetypes': 'javascript' }
NeoBundleLazy 'pangloss/vim-javascript', { 'filetypes': 'javascript' }
NeoBundleLazy 'othree/yajs.vim', { 'filetypes': 'javascript' }
NeoBundleLazy 'elzr/vim-json', { 'filetypes': 'json' }
NeoBundleLazy 'tpope/vim-haml', { 'filetypes': 'haml' }
NeoBundleLazy 'groenewege/vim-less', { 'filetypes': 'less' }
NeoBundleLazy 'slim-template/vim-slim', { 'filetypes': 'slim' }
NeoBundleLazy 'fatih/vim-go', { 'filetypes': 'go' }
NeoBundleLazy 'rust-lang/rust.vim', { 'filetypes': 'rust' }

" Colorscheme
" NeoBundle 'tomasr/molokai'
NeoBundle 'sickill/vim-monokai'
NeoBundle 'cdmedia/itg_flat_vim'
" NeoBundle 'sheerun/vim-wombat-scheme'

" Required:
call neobundle#end()

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

