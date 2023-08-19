local keymap = vim.keymap

--
-- Settings for Git related ops
--
keymap.set('n', '<leader>gn', ':Git ')
keymap.set('n', '<leader>ga', ':Git add .<cr>')
keymap.set('n', '<leader>gs', ':Git status<cr>')
keymap.set('n', '<leader>gsd', ':Gsdiff<cr>')
keymap.set('n', '<leader>gvd', ':Gvdiff<cr>')
keymap.set('n', '<leader>gb', ':Git blame<cr>')
keymap.set('n', '<leader>gc', ':Git commit -v<cr>')
keymap.set('n', '<leader>gsta', ':Git stash<cr>')
keymap.set('n', '<leader>gstp', ':Git stash pop<cr>')
keymap.set('n', '<leader>gstl', ':Git stash list<cr>')
keymap.set('n', '<leader>gp', ':Git push<cr>')
keymap.set('n', '<leader>gpf', ':Git push --force-with-lease<cr>')
