local map = vim.keymap.set

--Oil
map('n', '<Bslash>f', ':Oil .<CR>', { noremap = true, silent = true })
map('n', '<C-x>j', ':Oil .<CR>', { noremap = true, silent = true })

--Gitsigns toggle
map('n', '<leader>gs', ':Gitsigns toggle_signs<CR>', { noremap = true, silent = false })
map('n', '<leader>gb', ':Gitsigns blame<CR>', { noremap = true, silent = false })
map('n', '<C-,>', ':Gitsigns prev_hunk<CR>', { noremap = true, silent = false })
map('n', '<C-.>', ':Gitsigns next_hunk<CR>', { noremap = true, silent = false })
map('n', '<leader>gr', ':Gitsigns reset_hunk<CR>', { noremap = true, silent = false })
map('n', '<leader>g.', ':Gitsigns reset_buffer<CR>', { noremap = true, silent = false })
map('n', '<leader>gp', ':Gitsigns preview_hunk_inline<CR>', { noremap = true, silent = false })

--Mov to start/end of line
map('i', '<C-a>', '<ESC>I')
map('i', '<C-e>', '<ESC>A')

-- Always use very magic mode for searching
map('n', '/', [[/\v]])

-- Noh
map('n', '<c-l>', '<cmd>noh<CR>', { noremap = true, silent = true })

-- Escape -> jj
map('i', 'jj', '<Esc>', { nowait = true })
map('i', 'jj', '<Esc>', { nowait = true })
map('i', 'kk', '<C-Bslash><C-n>', { nowait = true })
map('i', 'kk', '<C-Bslash><C-n>', { nowait = true })

-- Escape terminal
map('t', 'jj', '<C-Bslash><C-n>', { nowait = true })
map('t', 'jj', '<C-Bslash><C-n>', { nowait = true })
map('t', 'kk', '<C-Bslash><C-n>', { nowait = true })
map('t', 'kk', '<C-Bslash><C-n>', { nowait = true })
map('t', '<Esc>', '<C-Bslash><C-n>', { nowait = true })

--Plugins file
map('n', '<F3>', ':Oil ~/.config/nvim/lua<CR>', { noremap = true, silent = true })

-- Telescope
map('n', '<Bslash><Bslash>', '<cmd>Telescope live_grep<CR>', { noremap = true, silent = true, nowait = true })
map('n', '<leader><leader>', '<cmd>Telescope find_files<CR>', { noremap = true, silent = true, nowait = true })
map('n', '<leader>p', '<cmd>Telescope projects theme=ivy<CR>', { noremap = true, silent = true, nowait = true })

--Source current file
map('n', '<leader>ss', '<cmd>so%<CR>', { noremap = true })

-- Neogit
map('n', '<leader>gg', '<cmd>Neogit<CR>', { noremap = true, silent = true })
map('n', '<C-x>g', '<cmd>Neogit<CR>', { noremap = true, silent = true })

--Tab navigation
map('n', '<A-Left>', ':bprevious<CR>', { silent = true })
map('n', '<A-Right>', ':bnext<CR>', { silent = true })
map('n', '<A-q>', ':wqa<CR>', { silent = true })
map('n', '<C-s>', ':silent w<CR><cmd>echo "Buffer saved"<CR>', { noremap = true, silent = true })
map('n', '<A-c>', ':bd<CR>', { silent = true })

--Resize tab
map('n', '<C-Left>', ':vertical resize -10<CR>', { silent = true })
map('n', '<C-Right>', ':vertical resize +10<CR>', { silent = true })

--Move lines up and down
map('n', '<C-Up>', ':m-2<CR>', { noremap = true, silent = true })
map('n', '<C-Down>', ':m+<CR>', { noremap = true, silent = true })

--Add lines above and below
map('n', '<A-Up>', ':put!=repeat(nr2char(10), v:count1)|silent ""]-<CR>', { noremap = true, silent = true })
map('n', '<A-Down>', ':put=repeat(nr2char(10), v:count1)|silent ""]+<CR>', { noremap = true, silent = true })

-- Change current working directory locally and print cwd after that
map('n', '<leader>.', '<cmd>lcd %:p:h<CR>', { noremap = true, silent = true })

-- Copy entire buffer.
map('n', '<leader>y', '<cmd>%yank<cr>', { noremap = true, silent = true })

-- Redo
map('n', 'U', '<C-r>', { noremap = true })

-- Do not move my cursor when joining lines.
map('n', 'J', function()
    vim.cmd([[
      normal! mzJ`z
      delmarks z
    ]])
end, {
    desc = 'join line',
})

-- Unsert semicolon in the end
map('i', '<A-;>', '<Esc>A;<Esc>i')

-- This is going to get me cancelled
vim.keymap.set('i', '<C-c>', '<Esc>')

-- Greatest remap ever
vim.keymap.set('x', 'p', [["_dP]])
map('n', 'p', '<cmd>pu<CR>')

-- Reload buffer
map('n', '<leader>e', '<cmd>e<CR><cmd>echo "Buffer reloaded"<CR>', { noremap = true, silent = true })

-- Recent files
map('n', '<C-r>', '<cmd>Telescope oldfiles<CR>', { noremap = true, silent = true })

-- Spectre
map('n', '<leader>,,', '<cmd>lua require("spectre").toggle()<CR>', {
    desc = 'Toggle Spectre',
})
map('n', '<leader>,w', '<cmd>lua require("spectre").open_visual({select_word=true})<CR>', {
    desc = 'Search current word',
})
map('n', '<leader>,f', '<cmd>lua require("spectre").open_file_search({select_word=true})<CR>', {
    desc = 'Search on current file',
})
map('v', '<leader>,w', '<esc><cmd>lua require("spectre").open_visual()<CR>', {
    desc = 'Search current word [visual]',
})
