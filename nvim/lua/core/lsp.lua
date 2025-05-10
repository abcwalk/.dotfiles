vim.lsp.config('yamlls', {
    settings = {

    }
})

vim.diagnostic.config({
    virtual_text = { current_line = true },
    update_in_insert = true,
    underline = false,
    severity_sort = true,
    float = {
        focusable = true,
        style = 'minimal',
        border = 'rounded',
        source = 'always',
        header = '',
        prefix = '',
    },
})

vim.lsp.enable({
    'lua_ls',
    'yamlls',
    'basedpyright',
    'bashls',
})
