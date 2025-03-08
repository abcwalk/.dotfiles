local status_ok, conform = pcall(require, 'conform')
if not status_ok then
    return
end

local slow_format_filetypes = {
    'go',
    'python',
    'sql',
}

conform.setup({
    formatters_by_ft = {
        lua = { 'stylua' },
        -- fortran = { 'fprettify' },
        go = {
            'goimports',
            'gofumpt',
            'goimports_reviser',
            'golines',
        },
        sql = { 'sqlfmt' },
        python = {
            -- 'ruff_format',
            -- 'ruff_organize_imports',
            -- 'ruff_fix',
            'autopep8',
            'isort',
            -- 'black',
        },
        css = { 'prettier' },
        svelte = { 'prettier' },
        c = { 'clang-format' },
        cpp = { 'clang-format' },
        java = { 'clang-format' },
        sh = { 'shfmt' },
        bash = { 'shfmt' },
        zsh = { 'shfmt' },
        javascript = { 'prettierd' },
        typescript = { 'prettierd' },
        -- ['*'] = { 'codespell' },
        ['_'] = { 'trim_whitespace' },
    },
    formatters = {
        --[[ RuBackup ]]
        autopep8 = {
            command = 'autopep8',
            args = { '--in-place', '--max-line-length=120', '$FILENAME' },
            stdin = false,
        },
        isort = {
            command = 'isort',
            args = { '--line-length=120', '-' },
            stdin = true,
        },
        -- ruff_organize_imports = {
        --     command = 'ruff',
        --     args = {
        --         'check',
        --         '--force-exclude',
        --         '--select=I001',
        --         '--fix',
        --         '--exit-zero',
        --         '--stdin-filename',
        --         '$FILENAME',
        --         '-',
        --     },
        --     stdin = true,
        --     cwd = require('conform.util').root_file({
        --         'pyproject.toml',
        --         'ruff.toml',
        --         '.ruff.toml',
        --     }),
        -- },
        -- ruff_format = {
        --     cwd = require('conform.util').root_file({
        --         'pyproject.toml',
        --         'ruff.toml',
        --         '.ruff.toml',
        --     }),
        -- },
    },
    format_after_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
            return
        end
        return { lsp_format = 'fallback' }
    end,
    notify_on_error = false,
})

vim.api.nvim_create_user_command('FormatDisable', function(args)
    if args.bang then
        -- FormatDisable! will disable formatting just for this buffer
        vim.b.disable_autoformat = true
    else
        vim.g.disable_autoformat = true
    end
end, {
    desc = 'Disable autoformat-on-save',
    bang = true,
})
vim.api.nvim_create_user_command('FormatEnable', function()
    vim.b.disable_autoformat = false
    vim.g.disable_autoformat = false
end, {
    desc = 'Re-enable autoformat-on-save',
})

vim.keymap.set('n', '<leader>cd', function()
    vim.cmd('FormatDisable')
    vim.cmd('echo "Autoformat disabled"')
end, { desc = 'Disable autoformat' })

vim.keymap.set('n', '<leader>ce', function()
    vim.cmd('FormatEnable')
    vim.cmd('echo "Autoformat enabled"')
end, { desc = 'Enable autoformat' })
