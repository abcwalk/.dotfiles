return {
    'stevearc/conform.nvim',
    opts = {},
    config = function()
        require('conform').setup({
            formatters_by_ft = {
                lua = { 'stylua' },
                go = { 'goimports', 'golines', 'gofmt' },
                python = { 'ruff_organize_imports', 'ruff_format' },
                sh = { 'shfmt' },
                bash = { 'shfmt' },
                zsh = { 'shfmt' },
                json = { 'prettier' },
                yaml = { 'prettier' },
                ['_'] = { 'trim_whitespace' },
            },
            formatters = {
                prettier = {
                    prepend_args = function()
                        return { '--single-quote' }
                    end,
                },
            },
            format_on_save = function(bufnr)
                if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
                    return
                end
                return { timeout_ms = 500, lsp_format = 'fallback' }
            end,
        })
    end,
}
