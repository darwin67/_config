" Plugin settings
lua require('plug')

" disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Configs with Lua
lua require('core')
lua require('core-keymap')
lua require('plugin-git')
lua require('plugin-nerdtree')

" Key Mappings
" ==================

" delete trailing whitespace on save
function StripTrailingWhitespace()
  " disable strip if the b:no_strip_whitespace is enabled
  if exists('b:no_strip_whitespace')
    return
  endif
  %s/\s\+$//ge
endfunction
autocmd BufWritePre * call StripTrailingWhitespace()

" toggle line number
function ToggleLineNumber()
  if &number
    set nonumber
  else
    set number
  endif
endfunction
nnoremap <leader>ln :call ToggleLineNumber()<cr>

