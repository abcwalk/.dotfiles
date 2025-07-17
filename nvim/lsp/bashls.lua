local blink = require('blink.cmp')

return {
    cmd = { 'bash-language-server' },
    filetypes = {
        'bash',
    },
    settings = {},
    capabilities = vim.tbl_deep_extend(
        'force',
        {},
        vim.lsp.protocol.make_client_capabilities(),
        blink.get_lsp_capabilities()
    ),
}
