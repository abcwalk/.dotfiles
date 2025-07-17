return {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    opts = {
        preset = 'modern',
        win = {
            title = true,
        },
        icons = {
            rules = false,
            breadcrumb = ' ', -- symbol used in the command line area that shows your active key combo
            separator = '󱦰  ', -- symbol used between a key and it's label
            group = '󰹍 ', -- symbol prepended to a group
        },
        plugins = {
            spelling = {
                enabled = false,
            },
        },
    },
    keys = {
        {
            '<Space>?',
            function()
                require('which-key').show({ global = false })
            end,
            desc = 'Buffer Local Keymaps (which-key)',
        },
    },
}
