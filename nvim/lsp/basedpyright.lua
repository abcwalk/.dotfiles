local blink = require('blink.cmp')

return {
    cmd = { 'basedpyright-langserver', '--stdio' },
    filetypes = { 'python' },
    root_markers = {
        'pyproject.toml',
        'setup.py',
        'setup.cfg',
        'requirements.txt',
        'Pipfile',
        'pyrightconfig.json',
        '.git',
    },
    settings = {
        basedpyright = {
            analysis = {
                autoSearchPaths = true,
                useLibraryCodeForTypes = true,
                typeCheckingMode = 'basic',
                diagnosticMode = 'openFilesOnly',
                exclude = {
                    '/.cache',
                    '/.mypy_cache',
                    '/.pytest_cache',
                    '/.ruff_cache',
                    '/.venv',
                    '/venv',
                    '/pycache',
                    '/dist',
                    '/node_modules',
                },
            },
        },
    },
    capabilities = vim.tbl_deep_extend(
        'force',
        {},
        vim.lsp.protocol.make_client_capabilities(),
        blink.get_lsp_capabilities()
    ),
}
