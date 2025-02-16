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
    json = { 'jsonlint' },
}

-- To allow other plugins to add linters to require('lint').linters_by_ft,

-- instead set linters_by_ft like this:
-- lint.linters_by_ft = lint.linters_by_ft or {}
-- lint.linters_by_ft['markdown'] = { 'markdownlint' }
--
-- However, note that this will enable a set of default linters,
-- which will cause errors unless these tools are available:
-- {
--   clojure = { "clj-kondo" },
--   dockerfile = { "hadolint" },
--   inko = { "inko" },
--   janet = { "janet" },
--   json = { "jsonlint" },
--   markdown = { "vale" },
--   rst = { "vale" },
--   ruby = { "ruby" },
--   terraform = { "tflint" },
--   text = { "vale" }
-- }
--
-- You can disable the default linters by setting their filetypes to nil:
-- lint.linters_by_ft['clojure'] = nil
-- lint.linters_by_ft['dockerfile'] = nil
-- lint.linters_by_ft['inko'] = nil
-- lint.linters_by_ft['janet'] = nil
-- lint.linters_by_ft['json'] = nil
-- lint.linters_by_ft['markdown'] = nil
-- lint.linters_by_ft['rst'] = nil
-- lint.linters_by_ft['ruby'] = nil
-- lint.linters_by_ft['terraform'] = nil
-- lint.linters_by_ft['text'] = nil

-- local flake8 = require('lint').linters.flake8
-- flake8.args = {
--     '--line-length=120',
-- }

-- Create autocommand which carries out the actual linting
-- on the specified events.
local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })
vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'InsertLeave' }, {
    group = lint_augroup,
    callback = function()
        require('lint').try_lint()
    end,
})

-- Set pylint to work in virtualenv
-- require('lint').linters.pylint.cmd = 'python'
-- require('lint').linters.pylint.args = {
--     '-m',
--     'pylint',
--     '-f',
--     'json',
--     '--init-hook',
--     "import sys; sys.path.insert(0, './modules')",
--     '--from-stdin',
--     function()
--         return vim.api.nvim_buf_get_name(0)
--     end,
-- }
