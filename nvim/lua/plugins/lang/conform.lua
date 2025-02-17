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
        fortran = { 'fprettify' },
        go = {
            'goimports',
            'gofumpt',
            'goimports_reviser',
            'golines',
        },
        sql = { 'sqlfmt' },
        -- python = {
        --     'ruff_format',
        --     'ruff_organize_imports',
        --     'ruff_fix',
        --     -- 'isort',
        --     -- 'black',
        -- },
        -- python = function(bufnr)
        --     if require('conform').get_formatter_info('ruff_format', bufnr).available then
        --         return {
        --             -- 'ruff_format',
        --             'ruff_organize_imports',
        --             'ruff_fix',
        --         }
        --     else
        --         return {
        --             'isort',
        --             'black',
        --         }
        --     end
        -- end,
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
        ruff_organize_imports = {
            command = 'ruff',
            args = {
                'check',
                '--force-exclude',
                '--select=I001',
                '--fix',
                '--exit-zero',
                '--stdin-filename',
                '$FILENAME',
                '-',
            },
            stdin = true,
            cwd = require('conform.util').root_file({
                'pyproject.toml',
                'ruff.toml',
                '.ruff.toml',
            }),
        },
        ruff_format = {
            cwd = require('conform.util').root_file({
                'pyproject.toml',
                'ruff.toml',
                '.ruff.toml',
            }),
        },
    },
    -- format_on_save = function(bufnr)
    --     -- Disable with a global or buffer-local variable
    --     if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
    --         return
    --     end
    --     return { timeout_ms = 500, lsp_format = 'fallback' }
    -- end,
    -- format_on_save = {
    --     timeout_ms = 500,
    --     lsp_format = 'fallback',
    -- },
    format_on_save = function(bufnr)
        if slow_format_filetypes[vim.bo[bufnr].filetype] then
            return
        end
        local function on_format(err)
            if err and err:match('timeout$') then
                slow_format_filetypes[vim.bo[bufnr].filetype] = true
            end
        end

        return { timeout_ms = 200, lsp_fallback = true }, on_format
    end,
    -- format_after_save = function(bufnr)
    --     if not slow_format_filetypes[vim.bo[bufnr].filetype] then
    --         return
    --     end
    --     return { lsp_fallback = true }
    -- end,
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
