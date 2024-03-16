local status_ok, conform = pcall(require, 'conform')
if not status_ok then
    return
end

local slow_format_filetypes = { 'go', 'python' }

conform.setup({
    formatters_by_ft = {
        lua = { 'stylua' },
        go = {
            'goimports',
            'gofumpt',
            'goimports_reviser',
            'golines',
            -- 'golangci-lint',
        },
        python = { 'black', 'isort' },
        bash = { 'beautysh' },
        ['*'] = { 'codespell' },
        ['_'] = { 'trim_whitespace' },
    },
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

    format_after_save = function(bufnr)
        if not slow_format_filetypes[vim.bo[bufnr].filetype] then
            return
        end
        return { lsp_fallback = true }
    end,
    notify_on_error = false,
})
