vim.cmd('colorscheme base16-black-metal-immortal')

vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'none' })
vim.api.nvim_set_hl(0, 'StatusLine', { bg = 'none' })
vim.api.nvim_set_hl(0, 'CursorLine', { bg = 'none' })
vim.api.nvim_set_hl(0, 'CursorLineNR', { link = 'String' })
vim.api.nvim_set_hl(0, 'LineNR', { bg = 'none' })
vim.api.nvim_set_hl(0, 'LineNR', { bg = 'none' })
vim.api.nvim_set_hl(0, 'SignColumn', { bg = 'none' })
vim.api.nvim_set_hl(0, 'SignifySignAdd', { bg = 'none', fg = 'NvimLightGreen' })
vim.api.nvim_set_hl(0, 'SignifySignChange', { bg = 'none', fg = 'NvimLightYellow' })
vim.api.nvim_set_hl(0, 'SignifySignDelete', { bg = 'none', fg = 'NvimLightRed' })
vim.api.nvim_set_hl(0, 'GitGutterAdd', { bg = 'none', fg = 'NvimLightGreen' })
vim.api.nvim_set_hl(0, 'GitGutterChange', { bg = 'none', fg = 'NvimLightYellow' })
vim.api.nvim_set_hl(0, 'GitGutterChangeDelete', { bg = 'none', fg = 'NvimLightRed' })
vim.api.nvim_set_hl(0, 'GitGutterDelete', { bg = 'none', fg = 'NvimLightRed' })
vim.api.nvim_set_hl(0, 'PMenu', { bg = 'none' })
vim.api.nvim_set_hl(0, '@comment.todo.comment', { bg = '#121212', fg = '#7799bb' })
vim.api.nvim_set_hl(0, '@comment.warning.comment', { bg = '#121212', fg = 'NvimLightYellow' })
vim.api.nvim_set_hl(0, '@comment.note.comment', { bg = '#121212', fg = 'NvimLightMagenta' })
vim.api.nvim_set_hl(0, '@comment.error.comment', { bg = '#121212', fg = 'NvimLightRed' })
vim.api.nvim_set_hl(0, 'CmpItemMenuDefault', { link = 'String' })

-- TODO
-- NOTE
-- WARN
-- BUG
-- XXX
-- PERF
