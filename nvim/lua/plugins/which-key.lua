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
        {
            '<leader>ca',
            '<cmd>%yank<cr>',
            desc = 'Copy entire buffer',
        },
        {
            '<leader>cp',
            function()
                local current_file = vim.fn.expand('%:p')
                vim.fn.setreg('+', current_file)
                print('Copied to clipboard: ' .. current_file)
            end,
            desc = 'Copy file path',
        },
        {
            '<leader>c.',
            function()
                vim.cmd('lcd %:p:h')
                vim.notify(vim.cmd('pwd'), vim.log.levels.INFO)
            end,
            desc = 'Change current directory to .',
        },
    },
}
