return {
    cmd = {
        'lua-language-server',
    },
    filetypes = {
        'lua',
    },
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' },
            },
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
        },
    },
    root_markers = {
        '.git',
        '.luacheckrc',
        '.luarc.json',
        '.luarc.jsonc',
        '.stylua.toml',
        'selene.toml',
        'selene.yml',
        'stylua.toml',
    },
    single_file_support = true,
    log_level = vim.lsp.protocol.MessageType.Warning,
}
