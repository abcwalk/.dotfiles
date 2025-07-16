return {
    cmd = { 'basedpyright' },
    filetypes = { 'python' },
    settings = {
        basedpyright = {
            disableOrganizeImports = true,
            analysis = {
                diagnosticMode = 'openFilesOnly',
                typeCheckingMode = 'off',
                autoSearchPaths = true,
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
}
