return {
    'nvim-treesitter/nvim-treesitter',
    version = false,
    build = ':TSUpdate',
    event = { 'BufReadPost', 'BufNewFile' },
    config = function()
        require('nvim-treesitter.configs').setup({
            modules = {},
            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
            },
            auto_install = true,
            ensure_installed = {
                'bash',
                'comment',
                'gitcommit',
                'go',
                'gosum',
                'gomod',
                'html',
                'json',
                'requirements',
                'lua',
                'markdown',
                'markdown_inline',
                'python',
                'vim',
                'vimdoc',
                'yaml',
            },
            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = '<leader>vv',
                    node_incremental = '+',
                    scope_incremental = false,
                    node_decremental = '_',
                },
            },
        })
    end,
}
