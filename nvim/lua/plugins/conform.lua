return {
    'stevearc/conform.nvim',
    opts = {},
    config = function()
        require('conform').setup({
            formatters_by_ft = {
                lua = { 'stylua' },
                go = { 'goimports', 'golines', 'gofmt' },
                python = {
                    'autopep8',
                    'isort',
                },
                sh = { 'shfmt' },
                bash = { 'shfmt' },
                zsh = { 'shfmt' },
                json = { 'prettier' },
                yaml = { 'prettier' },
                ['_'] = { 'trim_whitespace' },
            },
            format_on_save = {
                lsp_fallback = true,
                async = false,
            },
        })
    end,
}
