local oxocharcoal = require('oxocharcoal')

vim.o.background = 'dark'
vim.cmd('colorscheme oxocharcoal')

local white = '#d0d0d0'
local green = '#99ff99'
local orange = '#ffb897'

vim.api.nvim_set_hl(0, 'variable', { fg = white })
vim.api.nvim_set_hl(0, 'FloatBorder', { fg = white })
vim.api.nvim_set_hl(0, 'TelescopeBorder', { fg = white })
vim.api.nvim_set_hl(0, 'TelescopePromptBorder', { fg = oxocharcoal.base02, bg = oxocharcoal.base02 })
vim.api.nvim_set_hl(0, 'TelescopePromptNormal', { fg = oxocharcoal.base05, bg = oxocharcoal.base02 })
vim.api.nvim_set_hl(0, 'TelescopePromptPrefix', { fg = oxocharcoal.base08, bg = oxocharcoal.base02 })
vim.api.nvim_set_hl(0, 'TelescopeNormal', { fg = oxocharcoal.none, bg = oxocharcoal.blend })
vim.api.nvim_set_hl(0, 'TelescopePreviewTitle', { fg = oxocharcoal.base02, bg = oxocharcoal.base12 })
vim.api.nvim_set_hl(0, 'TelescopePromptTitle', { fg = oxocharcoal.base02, bg = oxocharcoal.base11 })
vim.api.nvim_set_hl(0, 'TelescopeResultsTitle', { fg = oxocharcoal.blend, bg = oxocharcoal.blend })
vim.api.nvim_set_hl(0, 'TelescopeSelection', { fg = oxocharcoal.none, bg = oxocharcoal.base02 })
vim.api.nvim_set_hl(0, 'TelescopePreviewLine', { fg = oxocharcoal.none, bg = oxocharcoal.base01 })
vim.api.nvim_set_hl(0, 'DiagnosticHint', { fg = green })
vim.api.nvim_set_hl(0, 'GitsignsAdd', { fg = green })
vim.api.nvim_set_hl(0, 'GitsignsChange', { fg = orange })
