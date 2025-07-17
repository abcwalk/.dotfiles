return {
    'mfussenegger/nvim-lint',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
        local lint = require('lint')
        lint.linters_by_ft = {
            -- yaml = { 'yamllint' },
            go = { 'golangcilint' },
            make = { 'checkmake' },
            python = { 'flake8' },
            dockerfile = { 'hadolint' },
            bash = { 'shellcheck' },
            json = { 'jsonlint' },
        }
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
