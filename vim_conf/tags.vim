"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ctags
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Ctags
let g:vim_tags_auto_generate = 1
if has('gui_macvim') || has("mac") || has("macunix")
    let g:vim_tags_ctags_binary = "/usr/local/bin/ctags"
endif

set tags=./tags;
set tags+=./*/tags;

" tagbar plugin
nnoremap <leader>t :TagbarToggle<CR>
