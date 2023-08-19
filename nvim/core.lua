-- vim.g.airline_powerfonts = 1
-- vim.g.airline_theme = 'onedark'

--
-- Core configurations
--
local set = vim.opt

-- Disable scrollbars
-- set.guioptions:remove('r')
-- set.guioptions:remove('R')
-- set.guioptions:remove('l')
-- set.guioptions:remove('L')

set.encoding = 'utf8'     -- set utf8 as standard encoding
set.history = 1000        -- the amount of history to preserve
set.autoread = true       -- set to autoread when a file is changed outside of Vim
set.wildmenu = true       -- turn on wild menu
set.ruler = true          -- always show the current position
set.cmdheight = 1         -- height of the command bar
set.hidden = true         -- a buffer becomes hidden when it's abandoned
set.ignorecase = true     -- ignore case when searching
set.smartcase = true      -- but also try to be smart when searching
set.hlsearch = true       -- highlight search results
set.incsearch = true      -- makes search act like search in modern browsers
set.lazyredraw = true     -- don't redraw while executing macros (good performance config)
set.magic = true          -- for regular expressions, turn magic on
set.showmatch = true      -- show matching brackets under the cursor
set.mat = 2               -- how many tenths of a second to blink when matching brackets
set.helpheight = 999      -- maximize help screen
set.confirm = true        -- confirm before closing file
set.scrolloff = 8         -- see at least 8 lines when scroliing up or down
set.paste = true          -- prevent extra indentation when copy and pasting
set.clipboard = 'unnamed' -- remap copy and paste to clipboard instead of buffer
set.smarttab = true       -- be smart when using tabs
set.shiftwidth = 2        -- 1 tab = 2 spaces
set.softtabstop = 2
set.tabstop = 2
set.expandtab = true
-- turn backup off since most stuff are in version control anyway
set.backup = false
set.wb = false
set.swapfile = false

set.backspace = { 'eol', 'start', 'indent' } -- configure backspace to act like one

set.whichwrap:append('<,>,h,l')

set.errorbells = false
set.visualbell = false

-- set.t_vb = '' -- not applicable in neovim

set.tm = 500                                                                           -- disable annoying sounds on errors
set.lbr = true
set.tw = 500                                                                           -- line break on 500 characters
set.autoindent = true
set.smartindent = true                                                                 -- nice indentation
set.wrap = true
set.wrapscan = true                                                                    -- wrap lines
set.list = true
set.listchars = { tab = '» ', trail = '-', extends = '»', precedes = '«', nbsp = '%' } -- make space visible
set.viminfo:prepend('%')                                                               -- remember info about open buffers on close
set.laststatus = 2                                                                     -- always show the status line
set.cursorline = true                                                                  -- highlight current cusor's location

-- enable mouse if it has one
if vim.fn.has('mouse') then
  set.mouse = 'a'
end
