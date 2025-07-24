return {
    'stevearc/conform.nvim',
    opts = {},
    config = function()
        require('conform').setup({
            formatters_by_ft = {
                lua = { 'stylua' },
                go = { 'goimports', 'golines', 'gofmt' },
                python = function(bufnr)
                    if require('conform').get_formatter_info('ruff_format', bufnr).available then
                        return { 'ruff_organize_imports' }
                    else
                        return { 'isort', 'autopep8' }
                    end
                end,
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
