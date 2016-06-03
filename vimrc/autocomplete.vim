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

