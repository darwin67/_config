"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
  endif

" Required:
set runtimepath^=$CONFIG/repos/github.com/Shougo/dein.vim

" set junk directory
let g:junkfile#directory = expand($CONFIG . "/tmp/junk")

" Required:
call dein#begin(expand('/home/darwin/_config'))

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Add or remove your plugins here:
call dein#add('Shougo/vimproc.vim', {
    \ 'build': {
    \     'windows' : 'tools\\update-dll-mingw',
    \     'cygwin'  : 'make -f make_cygwin.mak',
    \     'mac'     : 'make -f make_mac.mak',
    \     'linux'   : 'make',
    \     'unix': 'gmake',
    \    },
    \ })
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')
call dein#add('airblade/vim-gitgutter')
call dein#add('scrooloose/nerdtree')
call dein#add('Xuyuanp/nerdtree-git-plugin')
call dein#add('tpope/vim-fugitive')
call dein#add('Townk/vim-autoclose')
call dein#add('scrooloose/syntastic')
call dein#add('terryma/vim-multiple-cursors')
call dein#add('tomtom/tcomment_vim')
call dein#add('szw/vim-tags')
call dein#add('Shougo/neomru.vim')
call dein#add('Shougo/unite.vim', {
            \ 'depends' : 'neomru.vim'
            \ })

" Load on Insert
" -------------------------------------------------------------
call dein#add('Shougo/deoplete.nvim', {
            \ 'on_i': 1
            \ })

" Load on command
" -------------------------------------------------------------
call dein#add('rizzatti/dash.vim', {
            \ 'on_cmd' : [ 'Dash' ]
            \ })
call dein#add('majutsushi/tagbar', {
            \ 'on_cmd' : [ 'TagbarToggle' ]
            \ })
call dein#add('mileszs/ack.vim', {
            \ 'on_cmd' : [ 'Ack' ]
            \ })
" Doesn't work properly for some reason
" call dein#add('tyru/open-browser.vim')
" call dein#add('tyru/open-browser-github.vim', {
"             \ 'depends' : 'open-browser.vim',
"             \ 'on_cmd' : [ 'OpenGithubFile', 'OpenGithubPullReq' ]
"             \ })

" Load based on fileTypes
" -------------------------------------------------------------
call dein#add('vim-jp/vim-cpp', {
            \ 'on_ft' : [ 'c', 'cpp' ]
            \ })
call dein#add('tpope/vim-rails', {
            \ 'on_ft' : [ 'ruby' ]
            \ })
call dein#add('moll/vim-node', {
            \ 'on_ft' : [ 'javascript' ]
            \ })
call dein#add('pangloss/vim-javascript', {
            \ 'on_ft' : [ 'javascript' ]
            \ })
call dein#add('othree/yajs.vim', {
            \ 'on_ft' : [ 'javascript' ]
            \ })
call dein#add('elzr/vim-json', {
            \ 'on_ft' : [ 'json' ]
            \ })
call dein#add('tpope/vim-haml', {
            \ 'on_ft' : [ 'haml' ]
            \ })
call dein#add('groenewege/vim-less', {
            \ 'on_ft' : [ 'less' ]
            \ })
call dein#add('slim-template/vim-slim', {
            \ 'on_ft' : [ 'slim' ]
            \ })
call dein#add('fatih/vim-go', {
            \ 'on_ft' : [ 'go' ]
            \ })
call dein#add('rust-lang/rust.vim', {
            \ 'on_ft' : [ 'rust' ]
            \ })

" Thems
call dein#add('sickill/vim-monokai')
call dein#add('cdmedia/itg_flat_vim')

" You can specify revision/branch/tag.
" call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

" Required:
call dein#end()

" Required:
filetype plugin indent on

" If you want to install not installed plugins on startup.
if dein#check_install()
 call dein#install()
endif

"End dein Scripts-------------------------
