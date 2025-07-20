local api = vim.api

-- don't auto comment new line
api.nvim_create_autocmd('BufEnter', { command = [[set formatoptions-=cro]] })

-- go to last loc when opening a buffer
-- this mean that when you open a file, you will be at the last position
api.nvim_create_autocmd('BufReadPost', {
    callback = function()
        local mark = vim.api.nvim_buf_get_mark(0, '"')
        local lcount = vim.api.nvim_buf_line_count(0)
        if mark[1] > 0 and mark[1] <= lcount then
            pcall(vim.api.nvim_win_set_cursor, 0, mark)
        end
    end,
})

-- Enable spell checking for certain file types
api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
    pattern = { '*.txt', '*.md', '*.tex' },
    callback = function()
        vim.opt.spell = true
        vim.opt.spelllang = 'en'
    end,
})

-- close some filetypes with <q>
vim.api.nvim_create_autocmd('FileType', {
    group = vim.api.nvim_create_augroup('close_with_q', { clear = true }),
    pattern = {
        'PlenaryTestPopup',
        'help',
        'lspinfo',
        'man',
        'notify',
        'qf',
        'spectre_panel',
        'startuptime',
        'tsplayground',
        'neotest-output',
        'checkhealth',
        'neotest-summary',
        'neotest-output-panel',
    },
    callback = function(event)
        vim.bo[event.buf].buflisted = false
        vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, silent = true })
    end,
})

-- resize neovim split when terminal is resized
vim.api.nvim_command('autocmd VimResized * wincmd =')

vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('lsp-attach', { clear = true }),
    callback = function(event)
        local map = function(keys, func, desc)
            vim.keymap.set('n', keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
        end

        local wk = require('which-key')
        wk.add({
            { 'gd', vim.lsp.buf.definition, desc = 'Go to definition' },
            { 'gr', Snacks.picker.lsp_references, desc = 'Go to references' },
            { 'K', vim.lsp.buf.doc, desc = 'Hover doc' },
            { '<leader>la', vim.lsp.buf.code_action, desc = 'Code action' },
            { '<F2>', vim.lsp.buf.rename, desc = 'Rename' },
        })

        local function client_supports_method(client, method, bufnr)
            if vim.fn.has('nvim-0.11') == 1 then
                return client:supports_method(method, bufnr)
            else
                return client.supports_method(method, { bufnr = bufnr })
            end
        end

        local client = vim.lsp.get_client_by_id(event.data.client_id)
        client.server_capabilities.semanticTokensProvider = nil

        if client and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
            map('<leader>li', function()
                vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
            end, 'Toggle Inlay Hints')
        end
    end,
})

-- Show diagnostic on hover
vim.api.nvim_create_autocmd({ 'CursorHold' }, {
    pattern = '*',
    callback = function()
        for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
            if vim.api.nvim_win_get_config(winid).zindex then
                return
            end
        end
        vim.diagnostic.open_float({
            scope = 'cursor',
            focusable = false,
            close_events = {
                'CursorMoved',
                'CursorMovedI',
                'BufHidden',
                'InsertCharPre',
                'WinLeave',
            },
        })
    end,
})

vim.api.nvim_create_autocmd('ColorScheme', {
    group = vim.api.nvim_create_augroup('custom_highlights_gruvboxmaterial', {}),
    pattern = 'gruvbox-material',
    callback = function()
        local config = vim.fn['gruvbox_material#get_configuration']()
        local palette =
            vim.fn['gruvbox_material#get_palette'](config.background, config.foreground, config.colors_override)
        local set_hl = vim.fn['gruvbox_material#highlight']

        set_hl('FloatBorder', palette.none, palette.none)
        set_hl('FloatFooter', palette.none, palette.none)
        set_hl('FloatTitle', palette.none, palette.none)
        set_hl('NormalFloat', palette.none, palette.none)
        set_hl('Pmenu', palette.none, palette.none)
    end,
})

vim.api.nvim_create_autocmd('FileType', {
    callback = function()
        pcall(vim.treesitter.start)
    end,
})
