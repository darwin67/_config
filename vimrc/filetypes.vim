"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => FileType related settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Ruby
func! RubyConfig()
    call SetTabToTwoSpace()
endfunc


" Javascript
func! JSConfig()
    let javascript_enable_domhtmlcss=1
    let b:javascript_fold=0
    call SetTabToTwoSpace()
endfunc

" Golang  vim-go  (https://github.com/fatih/vim-go/wiki)
" Syntax-highlighting for Functions, Methods and Structs
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1

" Use Neosnippet for autocomplete
let g:go_snippet_engine = "neosnippet"

" Autoloads
augroup filetypes_settings
    autocmd!

    " C, C++
    autocmd BufRead,BufNewFile *.c setl filetype=c
    autocmd BufRead,BufNewFile *.cpp setl filetype=cpp

    " Ruby
    autocmd BufRead,BufNewFile *.rb setl filetype=ruby
    autocmd FileType ruby call RubyConfig()

    " Javascript
    autocmd BufRead,BufNewFile *.js setl filetype=javascript
    autocmd FileType javascript call JSConfig()

    " Markdown
    autocmd BufNewFile,BufReadPost *.md setl filetype=markdown

    " Haml
    autocmd BufRead,BufNewFile *.haml setl filetype=haml
    autocmd FileType haml call SetTabToTwoSpace()

    " Yaml
    autocmd BufRead,BufNewFile *.yml setl filetype=yaml

    " SQL
    autocmd BufRead,BufNewFile *.sql setl filetype=sql

    " Files I don't want white space to be deleted
    autocmd FileType vim let b:no_strip_whitespace=1

    " Strip all trailing white space when written
    autocmd BufWritePre * call StripTrailingWhitespace()
augroup END
