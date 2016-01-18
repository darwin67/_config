"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Powerline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if executable('powerline-daemon')
    python from powerline.vim import setup as powerline_setup
    python powerline_setup()
    python del powerline_setup

else
    let g:airline#extensions#tabline#enabled = 1
    let g:airline_powerline_fonts = 1
    if !exists('g:airline_symbols')
        let g:airline_symbols = {}
    endif
    let g:airline_symbols.space = "\ua0"

endif

set showtabline=2
set noshowmode

" let g:Powerline_cache_dir = simplify(expand($CONFIG . '/tmp/powerline'))
