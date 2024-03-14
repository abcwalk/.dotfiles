local status_ok, dracula = pcall(require, 'dracula')
if not status_ok then
    return
end

dracula.setup({
    transparent = true,
    -- on_colors = function(colors, color)
    --     return {
    --         -- override or create new colors
    --         mycolor = '#ffffff',
    --     }
    -- end,
    on_highlights = function(colors, color)
        return {
            -- Normal = { bg = 'none' },
            NormalFloat = { bg = 'none' },
            FloatBorder = { bg = 'none' },
            -- StatusLine = { bg = 'none' },
            CursorLine = { bg = 'none' },
            PMenu = { bg = 'none' },
            ['@variable.parameter'] = { italic = false },
            ['@lsp.type.parameter'] = { italic = false },
        }
    end,
    plugins = {
        ['nvim-treesitter'] = true,
        ['nvim-lspconfig'] = true,
        ['nvim-cmp'] = true,
        -- ['indent-blankline.nvim'] = true,
        -- ['neo-tree.nvim'] = true,
        -- ['nvim-tree.lua'] = true,
        -- ['which-key.nvim'] = true,
        -- ['dashboard-nvim'] = true,
        ['gitsigns.nvim'] = true,
        -- ['neogit'] = true,
        -- ['todo-comments.nvim'] = true,
        ['lazy.nvim'] = true,
        ['telescope.nvim'] = true,
        -- ['noice.nvim'] = true,
        -- ['hop.nvim'] = true,
        -- ['mini.statusline'] = true,
        -- ['mini.tabline'] = true,
        -- ['mini.starter'] = true,
        -- ['mini.cursorword'] = true,
    },
})

vim.cmd.colorscheme('dracula-soft')
