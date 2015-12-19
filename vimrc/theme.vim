"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors, Fonts and Theme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set font according to system
if has("mac") || has("macunix")
    set gfn=Meslo\ LG\ S\ for\ Powerline:h11,Source\ Code\ Pro:h12,Menlo:h11
elseif has("win16") || has("win32")
    set gfn=Meslo\ LG\ S\ for\ Powerline:h10,Source\ Code\ Pro:h11,Bitstream\ Vera\ Sans\ Mono:h11
elseif has("linux")
    set gfn=Meslo\ LG\ S\ for\ Powerline:h10,Source\ Code\ Pro:h11,Bitstream\ Vera\ Sans\ Mono:h11
elseif has("unix")
    set gfn=Meslo\ LG\ S\ for\ Powerline:h10
endif

" Disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

if &term =~ '256color'
    set t_ut=
endif

" Enable syntax highlighting
syntax enable

" Set background to dark
set background=dark

" Colorscheme
try
  colorscheme monokai
  " colorscheme molokai
  " colorscheme wombat
catch
  colorscheme itg_flat
endtry

let g:make = 'gmake'
if system('uname -o') =~ '^GNU/'
    let g:make = 'make'
endif

