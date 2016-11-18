"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerdtree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Mappings
nnoremap <leader>nt :NERDTree<CR>
nnoremap <leader>nm :NERDTreeMirror<CR>
nnoremap <leader>nc :NERDTreeClose<CR>

" Bookmark location
let g:NERDTreeBookmarksFile = expand("$CONFIG/tmp/NERDTree/.NERDTreeBookmarks")

" Nerdtree git plugin
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹",
    \ "Staged"    : "✚",
    \ "Untracked" : "✭",
    \ "Renamed"   : "➜",
    \ "Unmerged"  : "═",
    \ "Deleted"   : "✖",
    \ "Dirty"     : "✗",
    \ "Clean"     : "✔︎",
    \ "Unknown"   : "?"
    \ }

let g:NERDTreeIgnore = [ '\.sock$', '\.git$' ]

augroup nerdtree_group
    autocmd!
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
augroup END
