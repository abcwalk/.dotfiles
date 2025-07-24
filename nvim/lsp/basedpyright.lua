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
        '.gitignore',
        '.pylintrc',
    },
    capabilities = vim.tbl_deep_extend(
        'force',
        {},
        vim.lsp.protocol.make_client_capabilities(),
        blink.get_lsp_capabilities()
    ),
    settings = {
        basedpyright = {
            disableOrganizeImports = true,
            analysis = {
                exclude = {
                    '**/node_modules',
                    '**/__pycache__',
                    '/.cache',
                    '/.mypy_cache',
                    '/.pytest_cache',
                    '/.ruff_cache',
                    '/pycache',
                    '/dist',
                    '/node_modules',
                    '.venv',
                },
                extraPaths = { '**/framework' },
                autoImportCompletions = true,
                useLibraryCodeForTypes = true,
                typeCheckingMode = 'basic',
                diagnosticMode = 'openFilesOnly',
                autoSearchPaths = true,
            },
        },
    },
}
