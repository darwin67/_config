"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Syntastic
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_aggregate_errors = 1
" let g:syntastic_auto_jump = 3
let g:syntastic_auto_loc_list = 1

" Error and warning symbols
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_style_error_symbol = '✗'
let g:syntastic_style_warning_symbol = '⚠'

" Files syntastic should ignore
let g:syntastic_ignore_files = []

let b:active_filetypes = [
    \ "c",
    \ "cpp",
    \ "ruby",
    \ "javascript",
    \ "json",
    \ "sh",
    \ "go",
    \ "dockerfile"
\ ]

" Set filetypes to be check
let g:syntastic_mode_map = {
    \ "mode": "passive",
    \ "active_filetypes": b:active_filetypes,
\ }

" Specify checkers for filetype
let g:syntastic_c_checkers = [ 'gcc', 'cppcheck' ]
let g:syntastic_cpp_checkers = [ 'gcc', 'cppcheck' ]
let g:syntastic_ruby_checkers = [ 'rubocop' ]
let g:syntastic_javascript_checkers = [ 'jshint', 'eslint' ]
let g:syntastic_json_checkers = [ 'jsonlint' ]
let g:syntastic_css_checkers = [ 'recess' ]
let g:syntastic_less_checkers = [ 'recess' ]
let g:syntastic_scss_checkers = [ 'scss_lint' ]
let g:syntastic_slim_checkers = [ 'slim_lint' ]
let g:syntastic_dockerfile_checkers = [ 'dockerfile_lint' ]
let g:syntastic_sql_checkers = [ 'sqlint' ]
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_markdown_checkers = [ 'mdl' ]
let g:syntastic_sh_checkers = [ 'shellcheck' ]

" Auto commands
augroup rubocop_rails
    autocmd!
    au FileType ruby if exists('b:rails_root') |
        \ let b:syntastic_ruby_rubocop_options = '--rails' | endif
augroup END
