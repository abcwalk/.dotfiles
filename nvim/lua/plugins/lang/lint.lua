local status_ok, lint = pcall(require, 'lint')
if not status_ok then
    return
end

lint.linters_by_ft = {
    -- markdown = { 'markdownlint' },
    yaml = { 'yamllint' },
    -- go = { 'golangcilint' },
    make = { 'checkmake' },
    python = {
        'pylint',
        -- 'ruff',
    },
    javascript = { 'eslint_d' },
    dockerfile = { 'hadolint' },
    bash = { 'shellcheck' },
    -- json = { 'jsonlint' },
}

--[[ Pylint ]]
lint.linters.pylint.args = {
    '--disable',
    'missing-module-docstring',
    '--disable',
    'too-many-arguments',
    '--disable',
    'too-many-positional-arguments',
    '--disable',
    'too-many-locals',
    '--max-module-lines',
    '1200',
    '--max-line-length',
    '120',
    '-f',
    'json',
    '--from-stdin',
    function()
        return vim.api.nvim_buf_get_name(0)
    end,
}

local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })
vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'InsertLeave' }, {
    group = lint_augroup,
    callback = function()
        require('lint').try_lint()
    end,
})
