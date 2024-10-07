vim.keymap.set('n', '<C-[>', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<C-]>', vim.diagnostic.goto_next)

vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(event)
        local bufnr = event.buf
        local client = vim.lsp.get_client_by_id(event.data.client_id)
        client.server_capabilities.semanticTokensProvider = nil

        local map = function(keys, func)
            vim.keymap.set('n', keys, func, { buffer = event.buf })
        end

        map('<leader>f', '<cmd>Lspsaga finder tyd+ref+imp+def<CR>')
        map('<leader>d', '<cmd>Lspsaga finder def<CR>')
        map('<leader>i', '<cmd>Lspsaga finder imp<CR>')
        map('<leader>r', '<cmd>Lspsaga finder ref<CR>')
        map('K', '<cmd>Lspsaga hover_doc<CR>')
        map('<leader>ca', '<cmd>Lspsaga code_action<CR>')
        map('gD', '<cmd>Lspsaga peek_definition<CR>')
        map('gT', '<cmd>Lspsaga peek_type_definition<CR>')
        map('gd', '<cmd>Lspsaga goto_definition<CR>')
        map('<leader>q', '<cmd>Lspsaga show_workspace_diagnostics<CR>')
        map('<M-l>o', '<cmd>Lspsaga outline<CR>')
        map('<A-d>', '<cmd>Lspsaga term_toggle<CR>')
        map('<F2>', '<cmd>Lspsaga rename<CR>')

        if client.name == 'ruff' then
            -- Disable hover in favor of Pyright/basedpyright
            client.server_capabilities.hoverProvider = false
        end
    end,
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

local servers = {
    yamlls = {
        settings = {
            yaml = {
                completion = true,
                format = {
                    enable = true,
                    proseWrap = 'never',
                    printWidth = 200,
                },
                keyOrdering = false,
                -- schemaStore = {
                --     enable = true,
                --     url = 'https://www.schemastore.org/api/json/catalog.json',
                -- },
                -- schemas = require('schemastore').yaml.schemas(),
                validate = true,
            },
        },
    },
    lua_ls = {
        settings = {
            Lua = {
                diagnostics = {
                    -- Get the language server to recognize the `vim` global
                    globals = { 'vim' },
                },
                workspace = { checkThirdParty = false },
                telemetry = { enable = false },
                completion = {
                    callSnippet = 'Disable',
                },
                codelens = {
                    enable = true,
                },
            },
        },
    },
    basedpyright = {
        settings = {
            basedpyright = {
                disableOrganizeImports = true,
                analysis = {
                    diagnosticMode = 'openFilesOnly',
                    typeCheckingMode = 'standard',
                    autoSearchPaths = true,
                    exclude = {
                        '/.cache',
                        '/.mypy_cache',
                        '/.pytest_cache',
                        '/.ruff_cache',
                        '/.venv',
                        '/pycache',
                        '/dist',
                        '/node_modules',
                    },
                    -- ignore = { '*' },
                },
            },
        },
    },
    ruff = {
        init_options = {
            settings = {
                lint = {
                    enable = false,
                },
            },
        },
    },
    ts_ls = {},
    bashls = {},
}

require('mason').setup({
    ui = {
        check_outdated_packages_on_open = true,
        border = 'rounded',
        width = 0.8,
        height = 0.8,
    },
})

local ensure_installed = vim.tbl_keys(servers or {})
vim.list_extend(ensure_installed, {
    'basedpyright',
    'ts_ls',
    'bashls',
    'jsonls',
    'lua_ls',
    -- 'pyright',
    'stylua',
    -- 'black',
    'ruff',
    -- 'ruff-lsp',
    -- 'pylint',
    -- 'flake8',
    'hadolint',
    'vale',
    'markdownlint',
    'sqlfmt',
    'gopls',
    'gofumpt',
    'goimports',
    'eslint_d',
    'golines',
    'golangci-lint',
    'goimports-reviser',
    'prettierd',
    -- 'gomaps',
    'impl',
    -- 'isort',
    'shellcheck',
    'shfmt',
    'yamllint',
})
require('mason-tool-installer').setup({ ensure_installed = ensure_installed })

require('mason-lspconfig').setup({
    handlers = {
        function(server_name)
            local server = servers[server_name] or {}
            server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
            require('lspconfig')[server_name].setup(server)
        end,
    },
})

--- Signature Help
local cfg = {
    bind = true,
    max_width = 100,
    hint_enable = true, -- virtual hint enable
    -- floating_window = false,
    hint_prefix = '⚡️ ',
}

require('lsp_signature').setup(cfg)

--- UI
require('lspconfig.ui.windows').default_options.border = 'rounded'

vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = 'rounded',
})

vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = 'rounded',
})

vim.diagnostic.config({
    virtual_text = false,
    update_in_insert = false,
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

local signs = { Error = '✘', Warn = '', Hint = '', Info = '', Question = '' }
for type, icon in pairs(signs) do
    local hl = 'DiagnosticSign' .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
