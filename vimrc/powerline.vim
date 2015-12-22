"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Powerline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if executable('powerline-daemon')
    python from powerline.vim import setup as powerline_setup
    python powerline_setup()
    python del powerline_setup
endif

set showtabline=2
set noshowmode

" let g:Powerline_cache_dir = simplify(expand($CONFIG . '/tmp/powerline'))
