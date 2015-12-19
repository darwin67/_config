"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>gn :Git 
nnoremap <leader>ga :Git add .<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gsd :Gsdiff<CR>
nnoremap <leader>gvd :Gvdiff<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gc :Gcommit -v<CR>
nnoremap <leader>gh :Git stash<CR>
nnoremap <leader>gu :Git stash pop<CR>
nnoremap <leader>gp :Git push 

" Github browser
nnoremap <leader>pf :OpenGithubFile<CR>
nnoremap <leader>pa :OpenGithubPullReq<CR>
nnoremap <leader>pr :OpenGithubPullReq 
