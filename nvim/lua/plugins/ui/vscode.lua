local status_ok, vscode = pcall(require, 'vscode')
if not status_ok then
    return
end

vim.o.background = 'light'

local c = require('vscode.colors').get_colors()
require('vscode').setup({
    -- Alternatively set style in setup
    -- style = 'light'

    -- Enable transparent background
    transparent = false,

    -- Enable italic comment
    italic_comments = false,

    -- Underline `@markup.link.*` variants
    underline_links = false,

    -- Disable nvim-tree background color
    -- disable_nvimtree_bg = true,

    -- Override colors (see ./lua/vscode/colors.lua)
    -- color_overrides = {
    --     vscLineNumber = '#FFFFFF',
    -- },

    -- Override highlight groups (see ./lua/vscode/theme.lua)
    -- group_overrides = {
    -- this supports the same val table as vim.api.nvim_set_hl
    -- use colors from this colorscheme by requiring vscode.colors!
    -- Cursor = { fg = c.vscDarkBlue, bg = c.vscLightGreen, bold = true },
    -- },
})
require('vscode').load()
