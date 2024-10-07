local map = vim.keymap.set
vim.g.mapleader = ' '
-- local buffalo = require("buffalo.ui")
-- local dap = require("dap")
-- local dapui = require("dapui")

-- Buffalo
-- map({ "t", "n" }, "<C-e>", buffalo.toggle_buf_menu, { noremap = true })
-- map({ 't', 'n' }, '<C-t>', buffalo.toggle_tab_menu, { noremap = true })

-- NvimTree
-- map("n", "<leader>e", ":NvimTreeFindFileToggle<CR>", { noremap = true, silent = true })

-- Trouble
-- map("n", "<leader>d", function() require("trouble").open() end)
map('n', '<leader>q', '<cmd>TroubleToggle quickfix<cr>', { silent = true, noremap = true })

--PackerSync
-- map("n", "<leader>p", ":PackerSync<CR>", { noremap = true, silent = true, nowait = true })

--Zen-mode
-- map("n", "<leader>z", ":ZenMode<CR>", { noremap = true, silent = true })
-- map("n", "<leader>zz", function()
--   require("zen-mode").setup {
--     window = {
--       width = 90,
--       options = {}
--     },
--   }
--   require("zen-mode").toggle()
--   vim.wo.wrap = false
--   vim.wo.number = true
--   vim.wo.rnu = false
--   ApplyTheme()
-- end)

-- map("n", "<leader>zZ", function()
--   require("zen-mode").setup {
--     window = {
--       width = 80,
--       options = {}
--     },
--   }
--   require("zen-mode").toggle()
--   vim.wo.wrap = false
--   vim.wo.number = false
--   vim.wo.rnu = false
--   vim.opt.colorcolumn = "0"
--   ApplyTheme()
-- end)

--Lazy
-- map("n", "<leader>p", ":Lazy sync<CR>", { noremap = true, silent = true, nowait = true })

--ToggleTerm
map('n', '<C-Bslash>', ':ToggleTerm<CR>', { noremap = true, silent = true })

--Oil
map('n', '<Bslash>f', ':Oil .<CR>', { noremap = true, silent = true })

--- Neotree
-- map('n', '<Bslash>f', ':Neotree<CR>', { noremap = true, silent = true })

--Mini.files
-- map("n", "<Tab><Tab>", ":lua MiniFiles.open()<CR>", { noremap = true, silent = true })

-- Yabs
map('n', '<Tab><Tab>', ':YABSOpen<CR>:call cursor(1, 1)<CR>', { noremap = true, silent = true })

--Harpoon
-- map("n", "<leader>a", "<cmd>lua require('harpoon.mark').add_file()<CR><cmd>echo 'Harpoon: Mark added'<CR>",
--   { noremap = true, silent = true })
-- map("n", "<leader>e", ":lua require('harpoon.ui').toggle_quick_menu()<CR>", { noremap = true, silent = true })

--Window
-- map("n", "<leader>w", ":lua require('nvim-window').pick()<CR>", { silent = true })

--Neogen
-- map("n", "<leader>gf", ":lua require('neogen').generate({ type = 'func' })<CR>", { noremap = true, silent = true })
-- map("n", "<leader>gc", ":lua require('neogen').generate({ type = 'class' })<CR>", { noremap = true, silent = true })

--Goto-preview
-- map(
--   'n',
--   '<leader>d',
--   "<cmd>lua require('goto-preview').goto_preview_definition()<CR><cmd>",
--   { noremap = true, silent = true }
-- )
-- map(
--   'n',
--   '<leader>r',
--   "<cmd>lua require('goto-preview').goto_preview_references()<CR><cmd>",
--   { noremap = true, silent = true }
-- )
-- map('n', '<leader>q', "<cmd>lua require('goto-preview').close_all_win()<CR><cmd>", { noremap = true, silent = true })

--Cheatsheet
-- map("n", "<F12>", ":Cheatsheet<CR>", { noremap = truen, silent = true })

--Treesj
-- map("n", "<C-j>", function()
--   require("treesj").toggle { split = { recursive = true } }
-- end)

--Neotree
-- map("n", "<Bslash>f", ":NeoTreeRevealToggle<CR>", { noremap = true, silent = true })
-- map("n", "<Bslash>g", ":Neotree float git_status<CR>", { noremap = true, silent = true })
-- map("n", "<Tab><Tab>", ":Neotree float buffers<CR>", { noremap = true, silent = true })

--Alter-toggle false -> true, 1 -> 0, !== -> ===
-- map("n", "<M-r>", ":lua require('alternate-toggler').toggleAlternate()<CR>", { noremap = true, silent = true })

--Toggle-checkbox Markdown
map('n', '<leader>tt', ':ToggleCheckbox<CR>', { noremap = true, silent = true })

--Gitsigns toggle
map('n', '<leader>gs', ':Gitsigns toggle_signs<CR>', { noremap = true, silent = true })

--Floaterm
-- map("n", "<Bslash>t", ":FloatermToggle<CR>", { silent = true })
-- map("n", "<F5>", "<cmd>w<CR><cmd>echo 'Saved'<CR><cmd>FloatermNew! node %<CR>", { noremap = true, silent = true })

--Iron
-- map("n", "<leader>rs", ":IronRepl<CR>")
-- map("n", "<leader>rf", ":IronFocus<CR>")

--Trouble
-- map("n", "<leader>p", ":TroubleToggle<CR>")

--Mov to start/end of line
map('i', '<C-a>', '<ESC>I')
map('i', '<C-e>', '<ESC>A')

-- Always use very magic mode for searching
map('n', '/', [[/\v]])

-- Hlslens
-- map(
--     'n',
--     'n',
--     [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]],
--     { noremap = true, silent = true }
-- )
-- map(
--     'n',
--     'N',
--     [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]],
--     { noremap = true, silent = true }
-- )
-- map('n', '*', [[*<Cmd>lua require('hlslens').start()<CR>]], { noremap = true, silent = true })
-- map('n', '#', [[#<Cmd>lua require('hlslens').start()<CR>]], { noremap = true, silent = true })
-- map('n', 'g*', [[g*<Cmd>lua require('hlslens').start()<CR>]], { noremap = true, silent = true })
-- map('n', 'g#', [[g#<Cmd>lua require('hlslens').start()<CR>]], { noremap = true, silent = true })

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

--Open URL in browser (Windows)
-- vnoremap <silent> <C-F5> :<C-U>let old_reg=@"<CR>gvy:silent!!cmd /cstart <C-R><C-R>"<CR><CR>:let @"=old_reg<CR>
map('n', 'gx', ':silent !xdg-open <cfile><CR>', { noremap = true, silent = true }) -- Not work properly

-- Linux https://www.reddit.com/r/neovim/comments/ro6oye/comment/hq2o7rc/?utm_source=share&utm_medium=web2x&context=3
-- map("n", "gx", ":execute '!xdg-open ' .. shellescape(expand('<cfile>'), v:true)", { noremap = true, silent = true })

--Plugins file
map('n', '<F3>', ':Oil ~/.config/nvim/lua<CR>', { noremap = true, silent = true })

-- nvim-toggler emacs true
-- map("n", '<c-i>', require('nvim-toggler').toggle, { silent = true, nowait = true })

-- Telescope
map('n', '<Bslash><Bslash>', '<cmd>Telescope live_grep<CR>', { noremap = true, silent = true, nowait = true })
map('n', '<C-f>', '<cmd>Telescope find_files<CR>', { noremap = true, silent = true, nowait = true })
-- map('n', '<C-w>', '<cmd>Telescope grep_string<CR>', { noremap = true, silent = true, nowait = true })
map('n', '<leader>p', '<cmd>Telescope projects theme=ivy<CR>', { noremap = true, silent = true, nowait = true })
map('n', '<Tab><Tab>', '<cmd>Telescope buffers theme=ivy<CR>', { noremap = true, silent = true, nowait = true })

--Source current file
map('n', '<leader>ss', "<cmd>w | so%<CR><cmd>echo 'Sourced'<cr>", { noremap = true, nowait = true })

-- Neogit
map('n', '<C-g>', '<cmd>Neogit<CR>', { noremap = true, silent = true })

--Tab navigation
map('n', '<A-Left>', ':bprevious<CR>', { silent = true })
map('n', '<A-Right>', ':bnext<CR>', { silent = true })
-- Delete a buffer, without closing the window, see https://stackoverflow.com/q/4465095/6064933
-- map('n', '<A-d>', '<cmd>bprevious <bar> bdelete #<cr>', { silent = true })
-- map('n', '<A-q>', ':wqa<CR>', { silent = true })
map('n', '<C-s>', ':w<CR>')
-- map('n', '<A-w>', ':q<CR>', { silent = true })
map('n', '<A-q>', ':bdelete<CR>', { silent = true })
map('n', '<A-c>', ':bd<CR>', { silent = true })
-- map("n", "<A-d>", ":w <Bar> bdelete<CR>", { silent = true })

--Resize tab
map('n', '<C-Left>', ':vertical resize -10<CR>', { silent = true })
map('n', '<C-Right>', ':vertical resize +10<CR>', { silent = true })

--Move lines up and down
map('n', '<C-Up>', ':m-2<CR>', { noremap = true, silent = true })
map('n', '<C-Down>', ':m+<CR>', { noremap = true, silent = true })

--Add lines above and below
map('n', '<A-Up>', ':put!=repeat(nr2char(10), v:count1)|silent ""]-<CR>', { noremap = true, silent = true })
map('n', '<A-Down>', ':put=repeat(nr2char(10), v:count1)|silent ""]+<CR>', { noremap = true, silent = true })

-- Change current working directory locally and print cwd after that,
-- see https://vim.fandom.com/wiki/Set_working_directory_to_the_current_file
map('n', '<leader><leader>', '<cmd>lcd %:p:h<CR><cmd>pwd<CR>', { noremap = true, silent = false })

-- Copy entire buffer.
map('n', '<leader>y', "<cmd>%yank<cr><cmd>echo 'Copied all lines'<CR>")

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

-- Overseer
--map("n", '<F5>', ":w<CR>:echo 'Saved'<CR>:OverseerRun<CR>", { noremap = true, silent = true })

-- Close quickfix list
-- map("n", "<leader>q", ":cclose<CR>:echo 'Quickfix closed'<CR>", { noremap = true })

-- Swenv
-- map('n', '<leader>e', '<cmd>Swenv<CR>', { noremap = true, silent = true })

-- insert semicolon in the end
map('i', '<A-;>', '<Esc>A;<Esc>i')

-- This is going to get me cancelled
vim.keymap.set('i', '<C-c>', '<Esc>')

-- greatest remap ever
vim.keymap.set('x', 'p', [["_dP]])

-- map('n', '<F5>', function()
--   cmd("silent w")
--   dap.continue()
-- end)
-- map('n', '<F10>', dap.step_over)
-- map('n', '<F11>', dap.step_into)
-- map('n', '<F12>', dap.step_out)
-- map('n', '<leader>b', dap.toggle_breakpoint)
-- map("n", "<leader>cb", function()
--   dap.clear_breakpoints()
--   cmd("echo 'Breakpoints cleared'")
-- end)
-- map("n", "<leader>dd", dap.terminate)
-- map("n", "<leader>dl", function()
--   cmd("silent w")
--   dap.run_last()
--   cmd("echo 'Running last session'")
-- end)
-- map('n', '<leader>lp', function() dap.set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)

--Dapui
-- map('n', "<leader>du", dapui.toggle)

-- Neotree
-- map('n', '<S-f>', '<cmd>Neotree toggle reveal<CR>', { noremap = true })
-- map({ 't', 'n' }, '<C-e>', '<cmd>Neotree buffers<CR>', { noremap = true })
-- map({ 't', 'n' }, '<C-g>', '<cmd>Neotree source=git_status dir=%:p:h left<CR>', { noremap = true, nowait = true })
-- map('n', '<S-f>', '<cmd>Neotree position=left dir=%:p:h:h reveal_file=%:p<CR>')
-- mapn", "<leader>nc", ":Neotree close<CR>")

-- Spider
-- map({ 'n', 'o', 'x' }, 'w', "<cmd>lua require('spider').motion('w')<CR>", { desc = 'Spider-w' })
-- map({ 'n', 'o', 'x' }, 'e', "<cmd>lua require('spider').motion('e')<CR>", { desc = 'Spider-e' })
-- map({ 'n', 'o', 'x' }, 'b', "<cmd>lua require('spider').motion('b')<CR>", { desc = 'Spider-b' })
-- map({ 'n', 'o', 'x' }, 'ge', "<cmd>lua require('spider').motion('ge')<CR>", { desc = 'Spider-ge' })

-- Startify
-- map('n', '<leader>s', '<cmd>Startify<CR>', { noremap = true, silent = true, nowait = true })

-- Reload buffers
map('n', '<C-r>', '<cmd>bufdo e<CR><cmd>echo "Buffers reloaded"<CR>', { noremap = true, silent = true })

-- Spectre
map('n', '<F12>', '<cmd>lua require("spectre").toggle()<CR>', { noremap = true, silent = true })
map(
    'n',
    '<leader>.',
    '<cmd>lua require("spectre").open_visual({select_word=true})<CR>',
    { noremap = true, silent = true }
)
map('v', '<F12>', '<esc><cmd>lua require("spectre").open_visual()<CR>', { noremap = true, silent = true })
