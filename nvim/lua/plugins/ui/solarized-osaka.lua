local status_ok, solarized_osaka = pcall(require, 'solarized-osaka')
if not status_ok then
    return
end

solarized_osaka.setup({
    transparent = false,
    terminal_colors = true,
    styles = {
        comments = { italic = false },
        keywords = { italic = false },
        functions = {},
        variables = {},
        sidebars = 'dark',
        floats = 'dark',
    },
    sidebars = { 'qf', 'help' }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
    day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
    hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
    dim_inactive = false, -- dims inactive windows
    lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold

    on_highlights = function(hl, c)
        hl.Float = {
            bg = 'None',
        }
        hl.FloatBorder = {
            bg = 'None',
        }
        hl.Pmenu = {
            bg = 'None',
        }
        hl.OilVcsStatusModified = {
            fg = '#b28500',
            bg = 'None',
        }
        hl.OilVcsStatusDeleted = {
            fg = '#db302d',
            bg = 'None',
        }
        hl.OilVcsStatusAdd = {
            fg = '#849900',
            bg = 'None',
        }
    end,
})

vim.o.background = 'dark'
vim.cmd([[colorscheme solarized-osaka]])
