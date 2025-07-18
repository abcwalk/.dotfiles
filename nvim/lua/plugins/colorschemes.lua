return {
    {
        'sainnhe/gruvbox-material',
        enabled = false,
        priority = 1000,
        config = function()
            vim.g.gruvbox_material_transparent_background = 0
            vim.g.gruvbox_material_foreground = 'mix'
            vim.g.gruvbox_material_background = 'hard'
            vim.g.gruvbox_material_ui_contrast = 'high'
            vim.g.gruvbox_material_float_style = 'bright'
            vim.g.gruvbox_material_statusline_style = 'material'
            vim.g.gruvbox_material_cursor = 'auto'
            vim.cmd.colorscheme('gruvbox-material')
        end,
    },
    {
        'behemothbucket/alabaster.nvim',
        enabled = true,
        branch = 'custom', -- main | custom
        priority = 1000,
        config = function()
            vim.g.alabaster_dim_comments = true
            vim.g.alabaster_floatborder = true
            vim.cmd('colorscheme alabaster')
        end,
    },
}
