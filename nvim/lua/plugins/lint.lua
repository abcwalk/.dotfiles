return {
    'mfussenegger/nvim-lint',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
        local lint = require('lint')
        lint.linters_by_ft = {
            -- yaml = { 'yamllint' },
            -- go = { 'golangci-lint' },
            make = { 'checkmake' },
            python = { 'ruff' },
            dockerfile = { 'hadolint' },
            bash = { 'shellcheck' },
            json = { 'jsonlint' },
        }

        -- local flake8 = lint.linters.flake8
        -- flake8.args = {
        --     '-max-line-length',
        --     '120',
        -- }

        -- Auto-lint on save and text changes
        local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })

        vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'InsertLeave' }, {
            group = lint_augroup,
            callback = function()
                -- Only lint if linters are available for this filetype
                local linters = lint.linters_by_ft[vim.bo.filetype]
                if linters and #linters > 0 then
                    lint.try_lint()
                end
            end,
        })
    end,
}
