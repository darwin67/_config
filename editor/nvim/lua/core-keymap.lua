--
-- Key leader
--
vim.g.mapleader = ','

local keymap = vim.keymap
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd


-- Copy
keymap.set('n', 'y', '"*y')
keymap.set('n', 'yy', '"*yy')
keymap.set('v', 'Y', '"*Y')
keymap.set('v', 'y', '"*y')

-- Cut
keymap.set('n', 'dd', '"+dd')
keymap.set('n', 'D', '"+D')
keymap.set('v', 'D', '"+D')
keymap.set('v', 'X', '"+X')
keymap.set('v', 'd', '"+d')
keymap.set('v', 'x', '"+x')

-- Paste
keymap.set('n', 'p', '"*p')
keymap.set('n', 'P', '"*P')
keymap.set('n', 'gp', '"*gp')
keymap.set('n', 'gP', '"*gP')

-- Reload
-- keymap.set('n', '<leader>so', ':so $MYVIMRC')

-- window spliting
keymap.set('n', '<leader>-', ':split<cr>')
keymap.set('n', '<leader>|', ':vsplit<cr>')
autocmd('VimResized', {
  pattern = '*',
  command = 'wincmd ='
})

-- tabs
keymap.set('n', '<leader>tn', ':tabnew<cr>')
keymap.set('n', '<leader>to', ':tabonly<cr>')
keymap.set('n', '<leader>tc', ':tabclear<cr>')
keymap.set('n', '<leader>tm', ':tabmove')

-- opens a new tab with the current buffer's path
-- super useful when editing files in the same directory
-- keymap.set('n', '<leader>te', ':tabedit <c-r>=expand("%:p:h")<cr>/')
keymap.set('n', '<leader>te', ':tabedit')

-- switch cwd to the directory of the open buffer
keymap.set('n', '<leader>cd', ':cd %p:h<cr>:pwd<cr>')

-- return to last edit position when opening files
local last_edit = augroup('last_edit', { clear = true })
autocmd('BufReadPost', {
  pattern = '*',
  group = last_edit,
  command = [[
    if line("'\"") > 0 && line("'\"") <= line("$") |
      exe "normal! g`\"" |
    endif
  ]]
})

-- Move a line of text using ALT+[jk]
keymap.set('n', '<M-j>', 'mz:m+<cr>`z')
keymap.set('n', '<M-k>', 'mz:m-2<cr>`z')
keymap.set('v', '<M-j>', ':m\'>+<cr>`<my`>mzgv`yo`z')
keymap.set('v', '<M-k>', ':m\'<-2<cr>`>my`<mzgv`yo`z')

-- When you press gv you Ack after the selected text
keymap.set('v', '<silent>gv', ':call VisualSelection(\'gv\', \'\')')

-- Open Ack and put the cursor in the right position
-- TOOD: Use ripgrep instead
keymap.set('n', '<leader>a', ':Ack')

-- When you press <leader>r you can search and replace the selected text
keymap.set('v', '<silent><leader>r', ':call VisualSelection(\'replace\', \'\')<cr>')

local function toggle_line_numbers()
  if vim.wo.number then
    vim.wo.number = false
  else
    vim.wo.number = true
  end
end
keymap.set('n', '<leader>ln', toggle_line_numbers)
