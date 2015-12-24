"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Unite
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let maplocalleader = '-'

let g:unite_data_directory = $CONFIG . '/tmp/unite'

" neomru
let g:neomru#file_mru_path = $CONFIG . '/tmp/neomru/files'
let g:neomru#directory_mru_path = $CONFIG . '/tmp/neomru/directories'

" Menus
let g:unite_source_menu_menus = {}


" Default settings for Unite
let b:default_context = {
    \ 'prompt': '>> ',
    \ 'source_grep_max_candidates': 200,
    \ 'marked_icon': '✓',
    \ 'ignorecase': 1,
    \ 'smartcase': 1
\ }

let b:files_to_ignore = [
    \ '\.git/',
    \ 'bundle/',
    \ '\.bundle/',
    \ 'tmp/',
    \ 'modules/',
    \ 'log/',
    \ 'node_modules/',
    \ '\.sass-cache/',
    \ 'sandbox/',
    \ 'public/system/',
    \ 'public/assets/',
    \ 'public/calendars/',
    \ 'coverage/',
    \ 'spec/tmp/',
    \ 'vendor/bundle/',
    \ 'auto/',
    \ '\.cask/',
    \ 'eshell/',
    \ 'elpa/',
    \ '\$RECYCLE\.BIN/',
    \ '\.DS_Store',
    \ '\.rspec',
    \ 'tags',
    \ '.keep',
\ ]

" Use ack if the OS is Mac, otherwise use ack-grep
if $OS == "Mac"
    let b:ack = "ack"
else
    let b:ack = "ack-grep"
endif

" Settings for Unite grep (hw -> ag -> pt -> ack)
if executable('hw')
    " Use hw (highway)
    " https://github.com/tkengo/highway
    let g:unite_source_grep_command = 'hw'
    let g:unite_source_grep_default_opts = '--no-group --no-color'
    let g:unite_source_grep_recursive_opt = ''
elseif executable('ag')
    " Use ag (the silver searcher)
    " https://github.com/ggreer/the_silver_searcher
    let g:unite_source_grep_command = 'ag'
    let g:unite_source_grep_default_opts =
    \ '-i --vimgrep --hidden --ignore ' .
    \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
    let g:unite_source_grep_recursive_opt = ''
elseif executable('pt')
    " Use pt (the platinum searcher)
    " https://github.com/monochromegane/the_platinum_searcher
    let g:unite_source_grep_command = 'pt'
    let g:unite_source_grep_default_opts = '--nogroup --nocolor'
    let g:unite_source_grep_recursive_opt = ''
elseif executable(b:ack)
    " Use ack
    " http://beyondgrep.com/
    let g:unite_source_grep_command = b:ack
    let g:unite_source_grep_default_opts = '-i --no-heading --no-color -k -H'
    let g:unite_source_grep_recursive_opt = ''
endif


call unite#custom#profile('default', 'context', b:default_context)
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#source('file,file_mru,file_rec,file_rec/async,grep,locate',
                \ 'ignore_pattern', join(b:files_to_ignore, '\|'))


" Show root directories, files and buffer
nnoremap <leader>ff :Unite -start-insert file buffer<CR>

" Work like fuzzy finder
nnoremap <C-p> :Unite -start-insert file_rec/async<CR>

" Lookup most recent files that are used
nnoremap <leader>fr :Unite -start-insert file_mru<CR>

" Show buffer
nnoremap <leader>fb :Unite -quick-match buffer<CR>

" Git grep
nnoremap <leader>gg :Unite  grep/git:/:-iI\ --untracked\ --exclude-standard:<CR>

" Grep, but may use hw, ag, pt or ack based on executable
nnoremap <space><space> :Unite grep:.::<CR>

" See opened tabs
nnoremap <leader>ls :Unite tab<CR>

" Open Unite menus
nnoremap <leader>ma :Unite menu<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Unite Menus
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap [menu] <Nop>
nmap <localleader> [menu]

" Unite file operations
let g:unite_source_menu_menus.files = {
    \ 'description': 'Unite related commands                                     ⌘  -f',
\ }

let g:unite_source_menu_menus.files.command_candidates ={
    \ '▷ open file or buffer                                                ⌘  ,ff'   : 'Unite -start-insert file buffer',
    \ '▷ open file with recursive search (like fuzzy finder)                ⌘  <C-p>' : 'Unite -start-insert file_rec/async',
    \ '▷ open most recently used files                                      ⌘  ,fr'   : 'Unite -start-insert file_mru',
    \ '▷ edit new file                                                            '   : 'Unite file/new' ,
    \ '▷ show current directory                                                   '   : 'Unite output:pwd' ,
    \ '▷ reload .vimrc                                                      ⌘  ,so'   : 'normal ,so' ,
    \ '▷ toogle line number                                                 ⌘  ln '   : 'call ToggleLineNumber()' ,
\ }

nnoremap <silent>[menu]f :Unite -silent -start-insert menu:files<CR>


" Git operations
let g:unite_source_menu_menus.git = {
    \ 'description': 'Git related commands                                         ⌘  -g',
\ }

let g:unite_source_menu_menus.git.command_candidates = {
    \ '▷ git                       (fugitive)                               ⌘  ,gn'  : 'exec "Git" input("Git ")',
    \ '▷ git status                (fugitive)                               ⌘  ,gs'  : 'Gstatus',
    \ '▷ git add -all              (fugitive)                               ⌘  ,ga'  : 'Git add .',
    \ '▷ git commit                (fugitive)                               ⌘  ,gc'  : 'Gcommit -v',
    \ '▷ git diff -split           (fugitive)                               ⌘  ,gsd' : 'Gsdiff',
    \ '▷ git diff -vertical        (fugitive)                               ⌘  ,gvd' : 'Gvdiff',
    \ '▷ git blame                 (fugitive)                               ⌘  ,gb'  : 'Gblame',
    \ '▷ git rebase -master                                                       '  : 'Git rebase master',
    \ '▷ git rebase                                                               '  : 'exec "Git rebase" input("Git rebase ")',
    \ '▷ git rebase --continue                                                    '  : 'Git rebase --continue',
    \ '▷ git rebase --skip                                                        '  : 'Git rebase --skip',
    \ '▷ git checkout -master                                                     '  : 'Git checkout master',
    \ '▷ git checkout                                                             '  : 'exec "Git checkout" input("Git checkout ")',
    \ '▷ git stash                                                          ⌘  ,gh'  : 'Git stash',
    \ '▷ git stash pop                                                      ⌘  ,gu'  : 'Git stash pop',
    \ '▷ git push                                                           ⌘  ,gp'  : 'exec "Git push" input("Git push ")',
    \ '▷ open current file on github                                        ⌘  ,pf'  : 'OpenGithubFile',
    \ '▷ open pull request --all                                            ⌘  ,pa'  : 'OpenGithubPullReq',
    \ '▷ open pull request --with-number                                    ⌘  ,gh'  : 'exec "OpenGithubPullReq" input("PR number: ")',
\ }

nnoremap <silent>[menu]g :Unite -silent -start-insert menu:git<CR>


" Search operations
let g:unite_source_menu_menus.search = {
    \ 'description': 'File search related commands                              ⌘  -s',
\ }

let g:unite_source_menu_menus.search.command_candidates = {
    \ '▷ grep (hw -> ag -> pt -> ack)                                       ⌘  <space><space>' : 'Unite grep:.::',
    \ '▷ git grep                                                           ⌘  ,gg'            : 'Unite  grep/git:/:-iI\ --untracked\ --exclude-standard',
    \ '▷ ack                                                                ⌘  ,a'             : 'exec "Ack" input("Ack: ")',
\ }

nnoremap <silent>[menu]s :Unite -silent menu:search<CR>


" Neobundle operations
let g:unite_source_menu_menus.neobundle = {
    \ 'description': 'plugins administration with NeoBundle                  ⌘  -p',
\ }

" TODO: Add the following menus
"       - list
"       - docs
"       - check
let g:unite_source_menu_menus.neobundle.command_candidates = {
    \ '▷ neobundle'             : 'Unite neobundle',
    \ '▷ neobundle log'         : 'Unite neobundle/log',
    \ '▷ neobundle lazy'        : 'Unite neobundle/lazy',
    \ '▷ neobundle update'      : 'Unite neobundle/update',
    \ '▷ neobundle search'      : 'Unite neobundle/search',
    \ '▷ neobundle install'     : 'Unite neobundle/install',
    \ '▷ neobundle clean'       : 'NeoBundleClean',
    \ '▷ neobundle clear cache' : 'NeoBundleClearCache',
\ }

nnoremap <silent>[menu]p :Unite -silent menu:neobundle<CR>


" Navigation operations
let g:unite_source_menu_menus.navigation = {
    \ 'description': 'Tab and tree navigations                              ⌘  -n',
\ }

let g:unite_source_menu_menus.navigation.command_candidates = {
    \ '▷ show opened buffers                                                ⌘  ,fb'  : 'Unite -quick-match buffer',
    \ '▷ show opened tabs                                                   ⌘  ,ls'  : 'Unite tab',
    \ '▷ open nerdtree                                                      ⌘  ,nt'  : 'NERDTree',
    \ '▷ mirror nerdtree                                                    ⌘  ,nm'  : 'NERDTreeMirror',
    \ '▷ close nerdtree                                                     ⌘  ,nc'  : 'NERDTreeClose',
    \ '▷ toggle tagbar                                                      ⌘  ,tb'  : 'TagbarToggle',
    \ '▷ open new tab                                                       ⌘  ,tn'  : 'tabnew',
    \ '▷ close all tabs besides current one                                 ⌘  ,to'  : 'tabonly',
    \ '▷ close tab                                                          ⌘  ,tc'  : 'tabclose',
    \ '▷ move tab                                                           ⌘  ,tm'  : 'exec "tabmove" input("Move tab to: ")',
    \ '▷ edit tab                                                           ⌘  ,te'  : 'exec "tabedit" input("Path to file: ")',
    \ '▷ split window horizontally                                          ⌘  ,-'   : 'split',
    \ '▷ split window vertically                                            ⌘  ,|'   : 'vsplit',
\ }

nnoremap <silent>[menu]n :Unite -silent menu:navigation<CR>
