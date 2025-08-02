local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Move to diagnostic
map('n', '<C-[>', vim.diagnostic.goto_prev, opts)
map('n', '<C-]>', vim.diagnostic.goto_next, opts)

-- Oil
map('n', '<C-x>j', ':Oil .<CR>', opts)

-- Fyler
-- map('n', '<leader>e', '<cmd>Fyler<CR>', opts)

--Move to start/end of line
map('i', '<C-a>', '<ESC>I')
map('i', '<C-e>', '<ESC>A')

-- Always use very magic mode for searching
map('n', '/', [[/\v]])

-- Select all
map('n', '<C-a>', 'ggVG', { noremap = true, silent = false })

-- copy everything between { and } including the brackets
map('n', 'YY', 'va{y', opts)

-- Escape -> jj
map('i', 'jj', '<Esc>', { nowait = true })
map('i', 'jj', '<Esc>', { nowait = true })

-- Escape terminal
map('t', 'jj', '<C-Bslash><C-n>', { nowait = true })
map('t', 'jj', '<C-Bslash><C-n>', { nowait = true })
map('t', '<Esc>', '<C-Bslash><C-n>', { nowait = true })

-- Neogit
map('n', '<C-x>g', '<cmd>Neogit<CR>', opts)

-- Window navigation
map('n', '<A-Left>', '<C-w>h', opts)
map('n', '<A-Right>', '<C-w>l', opts)

-- Resize window
map('n', '<C-Left>', ':vertical resize -10<CR>', { silent = true })
map('n', '<C-Right>', ':vertical resize +10<CR>', { silent = true })

-- Save file
map('n', '<C-s>', ':silent w<CR>', opts)

-- Move lines up and down
map('n', '<C-Up>', ':m-2<CR>', opts)
map('n', '<C-Down>', ':m+<CR>', opts)

-- Add lines above and below
map('n', '<A-Up>', ':put!=repeat(nr2char(10), v:count1)|silent ""]-<CR>', opts)
map('n', '<A-Down>', ':put=repeat(nr2char(10), v:count1)|silent ""]+<CR>', opts)

-- Redo
map('n', 'U', '<C-r>', opts)

map('n', '<Esc>', ':nohlsearch<CR>', opts)

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

-- Insert copied text next line
map('n', 'p', ':put +<CR>', opts)
-- Replace text with copied text
-- map('v', 'p', '"_dP', opts)

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
