-- Jump between diagnostics
-- Независимо от LSP
vim.keymap.set('n', '<C-[>', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<C-]>', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>ca', require('actions-preview').code_actions)

vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
    callback = function(event)
        local bufnr = event.buf
        -- local client = vim.lsp.get_client_by_id(event.data.client_id)
        -- if client and client.supports_method('textDocument/inlayHint', { bufnr = bufnr }) then
        --     vim.lsp.inlay_hint.enable(bufnr, true)
        -- end

        -- NOTE: Remember that Lua is a real programming language, and as such it is possible
        -- to define small helper and utility functions so you don't have to repeat yourself.
        --
        -- In this case, we create a function that lets us more easily define mappings specific
        -- for LSP related items. It sets the mode, buffer and description for us each time.
        local map = function(keys, func)
            vim.keymap.set('n', keys, func, { buffer = event.buf })
        end

        map('<Space>f', '<cmd>Lspsaga finder tyd+ref+imp+def<CR>')
        map('<Space>d', '<cmd>Lspsaga finder def<CR>')
        map('<Space>i', '<cmd>Lspsaga finder imp<CR>')
        map('<Space>r', '<cmd>Lspsaga finder ref<CR>')
        map('K', '<cmd>Lspsaga hover_doc<CR>')
        map('<Space>ca', '<cmd>Lspsaga code_action<CR>')
        map('gD', '<cmd>Lspsaga peek_definition<CR>')
        map('gT', '<cmd>Lspsaga peek_type_definition<CR>')
        map('gd', '<cmd>Lspsaga goto_definition<CR>')
        map('<C-g>d', '<cmd>Lspsaga show_buf_diagnostics<CR>')
        map('<M-l>o', '<cmd>Lspsaga outline<CR>')
        map('<A-d>', '<cmd>Lspsaga term_toggle<CR>')
        map('<F2>', '<cmd>Lspsaga rename<CR>')

        -- Jump to the definition of the word under your cursor.
        --  This is where a variable was first declared, or where a function is defined, etc.
        --  To jump back, press <C-t>.
        -- map('<leader>d', require('telescope.builtin').lsp_definitions)

        -- Find references for the word under your cursor.
        -- map('<leader>r', require('telescope.builtin').lsp_references)

        -- Jump to the implementation of the word under your cursor.
        --  Useful when your language has ways of declaring types without an actual implementation.
        -- map('gI', require('telescope.builtin').lsp_implementations)

        -- Jump to the type of the word under your cursor.
        --  Useful when you're not sure what type a variable is and you want to see
        --  the definition of its *type*, not where it was *defined*.
        -- map('<leader>D', require('telescope.builtin').lsp_type_definitions)

        -- Fuzzy find all the symbols in your current document.
        --  Symbols are things like variables, functions, types, etc.
        -- map('<leader>ds', require('telescope.builtin').lsp_document_symbols)

        -- Quickfix menu Project
        -- map('<leader>q', require('telescope.builtin').diagnostics)

        -- Fuzzy find all the symbols in your current workspace.
        --  Similar to document symbols, except searches over your entire project.
        -- map('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols)

        -- Rename the variable under your cursor.
        --  Most Language Servers support renaming across files, etc.
        -- map('<F2', vim.lsp.buf.rename)

        -- Execute a code action, usually your cursor needs to be on top of an error
        -- or a suggestion from your LSP for this to activate.
        -- map('<leader>ca', vim.lsp.buf.code_action)
        -- map('<leader>ca', require('actions-preview').code_actions)

        -- Opens a popup that displays documentation about the word under your cursor
        --  See `:help K` for why this keymap.
        -- map('K', vim.lsp.buf.hover)

        -- WARN: This is not Goto Definition, this is Goto Declaration.
        --  For example, in C this would take you to the header.
        -- map('gD', vim.lsp.buf.declaration)

        -- The following two autocommands are used to highlight references of the
        -- word under your cursor when your cursor rests there for a little while.
        --    See `:help CursorHold` for information about when this is executed
        -- local client = vim.lsp.get_client_by_id(event.data.client_id)
        -- if client and client.server_capabilities.documentHighlightProvider then
        --     vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        --         buffer = event.buf,
        --         callback = vim.lsp.buf.document_highlight,
        --     })
        --
        --     vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
        --         buffer = event.buf,
        --         callback = vim.lsp.buf.clear_references,
        --     })
        -- end
    end,
})

-- LSP servers and clients are able to communicate to each other what features they support.
--  By default, Neovim doesn't support everything that is in the LSP specification.
--  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
--  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. Available keys are:
--  - cmd (table): Override the default command used to start the server
--  - filetypes (table): Override the default list of associated filetypes for the server
--  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
--  - settings (table): Override the default settings passed when initializing the server.
--        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/

local servers = {
    pyright = {
        autoImportCompletion = true,
        python = {
            analysis = {
                autoSearchPaths = true,
                diagnosticMode = 'openFilesOnly',
                indexing = true,
                typeCheckingMode = 'strict',
                useLibraryCodeForTypes = true,
            },
        },
    },
    yamlls = {
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
    gopls = {
        -- capabilities = {
        --     textDocument = {
        --         completion = {
        --             completionItem = {
        --                 commitCharactersSupport = true,
        --                 deprecatedSupport = true,
        --                 documentationFormat = { 'markdown', 'plaintext' },
        --                 preselectSupport = true,
        --                 insertReplaceSupport = true,
        --                 labelDetailsSupport = true,
        --                 snippetSupport = true,
        --                 resolveSupport = {
        --                     properties = {
        --                         'documentation',
        --                         'details',
        --                         'additionalTextEdits',
        --                     },
        --                 },
        --             },
        --             contextSupport = true,
        --             dynamicRegistration = true,
        --         },
        --     },
        -- },
        -- filetypes = { 'go', 'gomod', 'gosum', 'gotmpl', 'gohtmltmpl', 'gotexttmpl' },
        -- message_level = vim.lsp.protocol.MessageType.Error,
        -- cmd = {
        --     'gopls', -- share the gopls instance if there is one already
        --     -- '-remote.debug=:0',
        -- },
        -- root_dir = function(fname)
        --     local has_lsp, lspconfig = pcall(require, 'lspconfig')
        --     if has_lsp then
        --         local util = lspconfig.util
        --         return util.root_pattern('go.work', 'go.mod')(fname)
        --             or util.root_pattern('.git')(fname)
        --             or util.path.dirname(fname)
        --     end
        -- end,
        -- flags = { allow_incremental_sync = true, debounce_text_changes = 500 },
        -- settings = {
        --     gopls = {
        --         -- more settings: https://github.com/golang/tools/blob/master/gopls/doc/settings.md
        --         -- not supported
        --         analyses = {
        --             unreachable = true,
        --             nilness = true,
        --             unusedparams = true,
        --             useany = true,
        --             unusedwrite = true,
        --             ST1003 = true,
        --             undeclaredname = true,
        --             fillreturns = true,
        --             nonewvars = true,
        --             fieldalignment = false,
        --             shadow = true,
        --         },
        --         -- codelenses = {
        --         --     generate = true, -- show the `go generate` lens.
        --         --     gc_details = true, -- Show a code lens toggling the display of gc's choices.
        --         --     map = true,
        --         --     tidy = true,
        --         --     vendor = true,
        --         --     regenerate_cgo = true,
        --         --     upgrade_dependency = true,
        --         -- },
        --         hints = {
        --             -- assignVariableTypes = true,
        --             compositeLiteralFields = true,
        --             compositeLiteralTypes = true,
        --             constantValues = true,
        --             functionTypeParameters = true,
        --             parameterNames = true,
        --             -- rangeVariableTypes = true,
        --         },
        --         usePlaceholders = false,
        --         completeUnimported = true,
        --         staticcheck = true,
        --         matcher = 'Fuzzy',
        --         diagnosticsDelay = '500ms',
        --         symbolMatcher = 'fuzzy',
        --         semanticTokens = false,
        --         -- noSemanticTokens = true, -- WARN: disable semantic string tokens so we can use treesitter highlight injection
        --
        --         -- ['local'] = get_current_gomod(),
        --         -- gofumpt = _GO_NVIM_CFG.lsp_gofumpt or false, -- true|false, -- turn on for new repos, gofmpt is good but also create code turmoils
        --         -- buildFlags = { '-tags', 'integration' },
        --     },
        -- },
        -- -- NOTE: it is important to add handler to formatting handlers
        -- -- the async formatter will call these handlers when gopls responed
        -- -- without these handlers, the file will not be saved
        -- handlers = {
        --     ['range_format'] = function(...)
        --         vim.lsp.handlers[range_format](...)
        --         if vfn.getbufinfo('%')[1].changed == 1 then
        --             vim.cmd('noautocmd write')
        --         end
        --     end,
        --     ['formatting'] = function(...)
        --         vim.lsp.handlers[formatting](...)
        --         if vfn.getbufinfo('%')[1].changed == 1 then
        --             vim.cmd('noautocmd write')
        --         end
        --     end,
        -- },
        settings = {
            -- https://go.googlesource.com/vscode-go/+/HEAD/docs/settings.md#settings-for
            gopls = {

                analyses = {
                    -- nilness = true,
                    -- unusedparams = true,
                    -- unusedwrite = true,
                    -- useany = true,

                    unreachable = true,
                    nilness = true,
                    unusedparams = true,
                    useany = true,
                    unusedwrite = true,
                    ST1003 = true,
                    undeclaredname = true,
                    fillreturns = true,
                    nonewvars = true,
                    fieldalignment = false,
                    shadow = true,
                },
                experimentalPostfixCompletions = true,
                gofumpt = true,
                -- DISABLED: staticcheck
                --
                -- gopls doesn't invoke the staticcheck binary.
                -- Instead it imports the analyzers directly.
                -- This means it can report on issues the binary can't.
                -- But it's not a good thing (like it initially sounds).
                -- You can't then use line directives to ignore issues.
                --
                -- Instead of using staticcheck via gopls.
                -- We have golangci-lint execute it instead.
                --
                -- For more details:
                -- https://github.com/golang/go/issues/36373#issuecomment-570643870
                -- https://github.com/golangci/golangci-lint/issues/741#issuecomment-1488116634
                --
                -- staticcheck = true,
                usePlaceholders = false,
                -- hints = {
                -- assignVariableTypes = true,
                --[[ compositeLiteralFields = true,
                    compositeLiteralTypes = true,
                    constantValues = true,
                    functionTypeParameters = true,
                    parameterNames = true, ]]
                -- rangeVariableTypes = true,
                -- },
                codelenses = {
                    generate = true, -- show the `go generate` lens.
                    gc_details = true, -- Show a code lens toggling the display of gc's choices.
                    map = true,
                    tidy = true,
                    vendor = true,
                    regenerate_cgo = true,
                    upgrade_dependency = true,
                },
            },
        },
    },
    lua_ls = {
        settings = {
            Lua = {
                diagnostics = {
                    globals = { 'vim' },
                },
                hint = {
                    enable = false,
                },
                completion = {
                    callSnippet = 'Replace',
                },
                format = {
                    enable = true,
                    defaultConfig = {
                        indent_style = 'space',
                        indent_size = '4',
                    },
                },
            },
        },
    },
}

-- Ensure the servers and tools above are installed
--  To check the current status of installed tools and/or manually install
--  other tools, you can run
--    :Mason
--
--  You can press `g?` for help in this menu.
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
    'bashls',
    'gopls',
    'jsonls',
    'lua_ls',
    'pyright',
    'stylua',
    'black',
    'flake8',
    'vale',
    'markdownlint',
    -- 'gofumpt',
    -- 'goimports',
    -- 'golines',
    -- 'golangci-lint',
    -- 'goimports-reviser',
    -- 'gomaps',
    -- 'impl',
    'isort',
    'shellcheck',
    'shfmt',
    'yamllint',
})
require('mason-tool-installer').setup({ ensure_installed = ensure_installed })

local lsp = require('lspconfig')

require('mason-lspconfig').setup({
    handlers = {
        function(server_name)
            local server = servers[server_name] or {}
            -- This handles overriding only values explicitly passed
            -- by the server configuration above. Useful when disabling
            -- certain features of an LSP (for example, turning off formatting for tsserver)
            server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
            lsp[server_name].setup(server)
        end,
    },
})

--- UI

require('lspconfig.ui.windows').default_options.border = 'rounded'

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

vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = 'rounded',
})

vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = 'rounded',
})

local signs = { Error = '✘', Warn = '', Hint = '', Info = '', Question = '' }
for type, icon in pairs(signs) do
    local hl = 'DiagnosticSign' .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
