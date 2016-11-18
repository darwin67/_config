"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Called once right before you start selecting multiple cursors
func! Multiple_cursors_before()
    if exists(':NeoCompleteLock')==2
       exe 'NeoCompleteLock'
    endif
endfunc

" Called once only when the multiple selection is canceled (default
func! Multiple_cursors_after()
    if exists(':NeoCompleteUnlock')==2
        exe 'NeoCompleteUnlock'
    endif
endfunc

func! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunc

" Visual selection for Ack
func! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("Ack \"" . l:pattern . "\" " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunc

" Returns true if paste mode is enabled
func! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunc

" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
func! <SID>BufcloseCloseIt()
   let l:currentBufNum = bufnr("%")
   let l:alternateBufNum = bufnr("#")

   if buflisted(l:alternateBufNum)
     buffer #
   else
     bnext
   endif

   if bufnr("%") == l:currentBufNum
     new
   endif

   if buflisted(l:currentBufNum)
     execute("bdelete! ".l:currentBufNum)
   endif
endfunc

" Delete trailing white space on save
func! StripTrailingWhitespace()
    " Strip only if the b:no_strip_whitespace is enabled
    if exists('b:no_strip_whitespace')
        return
    endif
    %s/\s\+$//ge
endfunc

" Toggle line number
func! ToggleLineNumber()
    if &number
        set nonumber
    else
        set number
    endif
endfunc

func! SetTabToTwoSpace()
    setl shiftwidth=2 softtabstop=2 tabstop=2
endfunc
