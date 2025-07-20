return {
    'nvim-lualine/lualine.nvim',
    config = function()
        require('lualine').setup({
            options = {
                theme = 'auto',
                globalstatus = true,
                component_separators = { left = '', right = '' },
                section_separators = { left = '', right = '' },
                always_divide_middle = true,
                disabled_filetypes = {
                    statusline = { 'alpha', 'NvimTree', 'trouble', 'Outline' },
                },
            },
            sections = {
                lualine_a = {},
                lualine_b = {
                    {
                        'branch',
                        icons_enabled = true,
                        icon = '',
                    },
                },
                lualine_c = {
                    {
                        'filename',
                        path = 4,
                    },
                },
                lualine_x = {},
                lualine_y = {
                    {
                        'location',
                    },
                },
                lualine_z = {
                    {
                        'lsp_status',
                        icon = '',
                        symbols = {
                            spinner = { '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏' },
                            done = '✓',
                            separator = ' ',
                        },
                        ignore_lsp = {},
                    },
                },
            },
        })
    end,
}
