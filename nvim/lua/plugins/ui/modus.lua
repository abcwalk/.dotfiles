local status_ok, modus = pcall(require, 'modus-themes')
if not status_ok then
    return
end

-- Default options
modus.setup({
    -- Theme comes in two styles `modus_operandi` and `modus_vivendi`
    -- `auto` will automatically set style based on background set with vim.o.background
    style = 'modus_vivendi',
    variant = 'default',  -- Theme comes in four variants `default`, `tinted`, `deuteranopia`, and `tritanopia`
    transparent = true,   -- Transparent background (as supported by the terminal)
    dim_inactive = false, -- "non-current" windows are dimmed
    styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = false },
        keywords = { italic = false },
        functions = {},
        variables = {},
    },

    --- You can override specific color groups to use other groups or a hex color
    --- function will be called with a ColorScheme table
    ---@param colors ColorScheme
    on_colors = function(colors) end,

    --- You can override specific highlights to use other groups or a hex color
    --- function will be called with a Highlights and ColorScheme table
    ---@param highlights Highlights
    ---@param colors ColorScheme
    on_highlights = function(highlights, colors) end,
})

vim.cmd([[colorscheme modus_vivendi]])
