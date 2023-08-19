local keymap = vim.keymap
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

--
-- Nerdtree settings
--

keymap.set('n', '<leader>ll', ':NERDTreeToggle<cr>')
-- keymap.set('n', '<leader>nm', ':NERDTreeMirror<cr>')

vim.g.NERDTreeIndicatorMapCustom = {
  Modified  = "✹",
  Staged    = "✚",
  Untracked = "✭",
  Renamed   = "➜",
  Unmerged  = "═",
  Deleted   = "✖",
  Dirty     = "✗",
  Clean     = "✔︎",
  Unknown   = "?"
}

vim.g.NERDTreeIgnore = { '.sock$', '.git$' }


local nerdtree = augroup('nerdtree_group', { clear = true })
autocmd('bufenter', {
  pattern = '*',
  group = nerdtree,
  command = 'if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif'
})
