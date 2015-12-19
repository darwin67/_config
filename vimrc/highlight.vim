"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Line and buffer highlight settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" High light current line
augroup LineHighLight
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter * setl cul
    autocmd WinLeave * setl nocul
augroup END
