return {
    'nvim-lualine/lualine.nvim',
    config = function()
        require('lualine').setup({
            options = {
                globalstatus = true,
                component_separators = { left = '', right = '' },
                section_separators = { left = '', right = '' },
                always_divide_middle = true,
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
                lualine_x = {
                    { 'lsp_progress' },
                    { 'diagnostics' },
                },
                lualine_y = {
                    {
                        'location',
                    },
                },
                lualine_z = {
                    {
                        'lsp_status',
                    },
                },
            },
        })
    end,
}
