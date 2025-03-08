local api = vim.api

-- autochdir
-- vim.api.nvim_create_autocmd('BufEnter', {
--     pattern = '*',
--     callback = function()
--         vim.cmd('silent! lcd ' .. vim.fn.expand('%:p:h'))
--     end,
-- })

-- api.nvim_create_autocmd('FileType', {
--   group = vim.api.nvim_create_augroup('grug-far-keybindings', { clear = true }),
--   pattern = { 'grug-far' },
--   callback = function()
--     vim.api.nvim_buf_set_keymap(0, 'n', '<C-enter>', '<localleader>o<localleader>q', {})
--   end,
-- })

--Remember last cursor position
api.nvim_create_autocmd('BufRead', {
    callback = function(opts)
        api.nvim_create_autocmd('BufWinEnter', {
            once = true,
            buffer = opts.buf,
            callback = function()
                local ft = vim.bo[opts.buf].filetype
                local last_known_line = api.nvim_buf_get_mark(opts.buf, '"')[1]
                if
                    not (ft:match('commit') and ft:match('rebase'))
                    and last_known_line > 1
                    and last_known_line <= api.nvim_buf_line_count(opts.buf)
                then
                    api.nvim_feedkeys([[g`"]], 'x', false)
                end
            end,
        })
    end,
})

--Remove all trailing whitespaces on save for all filetypes
--We can use .editorconfig to format
-- api.nvim_create_autocmd({ 'BufWritePre' }, {
--     pattern = { '*' },
--     callback = function()
--         local save_cursor = vim.fn.getpos('.')
--         vim.cmd([[%s/\s\+$//e]])
--         vim.fn.setpos('.', save_cursor)
--     end,
-- })

-- api.nvim_create_autocmd('FileType', {
-- pattern = { 'python' },
-- callback = function()
-- require('swenv.api').auto_venv()
-- require('swenv.api').set_venv('autotests')
-- end,
-- })

-- vim.api.nvim_create_autocmd('LspAttach', {
--     desc = 'LSP actions',
--     callback = function(event)
--         local opts = { buffer = event.buf }
--         vim.keymap.set('n', '<Space>f', '<cmd>Lspsaga finder imp+def+ref<CR>', opts)
--         vim.keymap.set('n', '<Space>d', '<cmd>Lspsaga finder def<CR>', opts)
--         vim.keymap.set('n', '<Space>i', '<cmd>Lspsaga finder imp<CR>', opts)
--         vim.keymap.set('n', '<Space>r', '<cmd>Lspsaga finder ref<CR>', opts)
--         vim.keymap.set('n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts)
--         vim.keymap.set({ 'n', 'v' }, '<Space>ca', '<cmd>Lspsaga code_action<CR>', opts)
--         vim.keymap.set('n', '<Space>r', '<cmd>Lspsaga finder ref<CR>', opts)
--         vim.keymap.set('n', 'gD', '<cmd>Lspsaga peek_definition<CR>', opts)
--         vim.keymap.set('n', 'gT', '<cmd>Lspsaga peek_type_definition<CR>', opts)
--         vim.keymap.set('n', 'gd', '<cmd>Lspsaga goto_definition<CR>', opts)
--         vim.keymap.set('n', '<C-g>d', '<cmd>Lspsaga show_buf_diagnostics<CR>', opts)
--         vim.keymap.set('n', '<M-l>o', '<cmd>Lspsaga outline<CR>', opts)
--         vim.keymap.set('n', '<C-[>', '<cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
--         vim.keymap.set('n', '<C-]>', '<cmd>Lspsaga diagnostic_jump_next<CR>', opts)
--         vim.keymap.set({ 'n', 't' }, '<A-d>', '<cmd>Lspsaga term_toggle<CR>', opts)
--         vim.keymap.set({ 'n', 't' }, '<F2>', '<cmd>Lspsaga rename<CR>', opts)
--     end,
-- })

-- Format on save
-- vim.api.nvim_create_autocmd('BufWritePre', {
--   callback = function()
--     vim.lsp.buf.format({ async = false })
--   end,
-- })

-- Go
-- local format_sync_grp = vim.api.nvim_create_augroup('GoFormat', {})
-- vim.api.nvim_create_autocmd('BufWritePre', {
--     pattern = '*.go',
--     callback = function()
--         require('go.format').goimport()
--     end,
--     group = format_sync_grp,
-- })

--Highlight on yank
-- api.nvim_create_augroup('YankHighlightGrp', {})
-- api.nvim_create_autocmd('TextYankPost', {
--     group = 'YankHighlightGrp',
--     pattern = '*',
--     callback = function()
--         vim.highlight.on_yank({
--             higroup = 'IncSearch',
--             timeout = 200,
--         })
--     end,
-- })

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
-- api.nvim_create_autocmd('TextYankPost', {
--     desc = 'Highlight when yanking (copying) text',
--     group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
--     callback = function()
--         vim.highlight.on_yank()
--     end,
-- })

--Disable autocomments
vim.api.nvim_create_autocmd('BufEnter', {
    pattern = '*',
    callback = function()
        vim.opt.formatoptions = vim.opt.formatoptions - { 'c', 'r', 'o' }
    end,
})

-- vim.api.nvim_create_autocmd('FileType', {
--     pattern = 'oil',
--     callback = function()
--         vim.cmd('set nonumber')
--     end,
-- })

-- close some filetypes with <q>
local function augroup(name)
    return vim.api.nvim_create_augroup('nvim_' .. name, { clear = true })
end

vim.api.nvim_create_autocmd('FileType', {
    group = augroup('close_with_q'),
    pattern = {
        'PlenarymapPopup',
        'help',
        'lspinfo',
        'man',
        'notify',
        'qf',
        'spectre_panel',
        'startuptime',
        'tsplayground',
        'neomap-output',
        'checkhealth',
        'neomap-summary',
        'neomap-output-panel',
        '[no name]',
    },
    callback = function(event)
        vim.bo[event.buf].buflisted = false
        vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, silent = true })
    end,
})

-- Toggle diagnostic dependent of insert mode
-- vim.api.nvim_create_autocmd('InsertEnter', {
--     desc = 'Hide diagnostic messages in insert mode',
--     callback = function()
--         vim.diagnostic.disable()
--     end,
-- })
--
-- vim.api.nvim_create_autocmd('InsertLeave', {
--     desc = 'Show diagnostic messages in normal mode',
--     callback = function()
--         vim.diagnostic.enable()
--     end,
-- })

-- Delete [No Name] buffers
vim.api.nvim_create_autocmd('BufHidden', {
    desc = 'Delete [No Name] buffers',
    callback = function(data)
        if data.file == '' and vim.bo[data.buf].buftype == '' and not vim.bo[data.buf].modified then
            vim.schedule(function()
                pcall(vim.api.nvim_buf_delete, data.buf, {})
            end)
        end
    end,
})

-- Inlay hint
-- if vim.fn.has('nvim-0.10.0') == 1 then
--     vim.api.nvim_create_autocmd('LspAttach', {
--         desc = 'Enable inlayHint feature',
--         callback = function(args)
--             local bufnr = args.buf
--             local client = vim.lsp.get_client_by_id(args.data.client_id)
--             if client and client.supports_method('textDocument/inlayHint', { bufnr = bufnr }) then
--                 vim.lsp.inlay_hint.enable(bufnr, true)
--             end
--         end,
--     })
-- end

-- Illuminate auto update the highlight style on colorscheme change
-- vim.api.nvim_create_autocmd({ 'ColorScheme' }, {
--     pattern = { '*' },
--     callback = function()
--         vim.api.nvim_set_hl(0, 'IlluminatedWordText', { link = 'Visual' })
--         vim.api.nvim_set_hl(0, 'IlluminatedWordRead', { link = 'Visual' })
--         vim.api.nvim_set_hl(0, 'IlluminatedWordWrite', { link = 'Visual' })
--     end,
-- })

-- vim.api.nvim_create_autocmd("WinEnter", {
--   callback = function()
--     local floating = vim.api.nvim_win_get_config(0).relative ~= ""
--     vim.diagnostic.config({
--       virtual_text = floating,
--       virtual_lines = not floating,
--     })
--   end,
-- })

-- Delete [No Name] buffers.
-- api.nvim_create_autocmd("BufHidden", {
--   desc = "Delete [No Name] buffers",
--   callback = function(event)
--     if event.file == "" and vim.bo[event.buf].buftype == "" and not vim.bo[event.buf].modified then
--       vim.schedule(function() pcall(vim.api.nvim_buf_delete, event.buf, {}) end)
--     end
--   end,
-- })

-- Add this autocmd to exit yabs when mouse click the main buffer.
-- api.nvim_create_autocmd("BufEnter", {
--   pattern = { "*" },
--   callback = function()
--     if vim.bo.buflisted then
--       require 'yabs'.leave()
--     end
--   end,
-- })

-- Function to check if a floating dialog exists and if not
-- then check for diagnostics under the cursor
-- function OpenDiagnosticIfNoFloat()
--   for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
--     if vim.api.nvim_win_get_config(winid).zindex then
--       return
--     end
--   end
--   -- THIS IS FOR BUILTIN LSP
--   vim.diagnostic.open_float(0, {
--     scope = "cursor",
--     focusable = false,
--     close_events = {
--       "CursorMoved",
--       "CursorMovedI",
--       "BufHidden",
--       "InsertCharPre",
--       "WinLeave",
--     },
--   })
-- end
-- vim.api.nvim_create_autocmd({ 'CursorHold' }, {
--     callback = function()
--         if vim.lsp.buf.server_ready() then
--             vim.diagnostic.open_float()
--         end
--     end,
-- })

-- vim.api.nvim_create_autocmd("BufWritePre", {
--   callback = function()
--     vim.lsp.buf.format { async = false }
--   end
-- })

-- JAVA
-- local _jdtls, jdtls = pcall(require, "lsp.jdtls")
-- if _jdtls and type(jdtls) ~= "boolean" then
-- 	vim.api.nvim_create_autocmd({ "FileType" }, {
-- 		pattern = "java",
-- 		callback = jdtls.start,
-- 		desc = "Starting Java language server",
-- 	})
-- end

--One statusline for split Terminal and buffer
-- cmd "autocmd TermOpen * setlocal nonumber norelativenumber | set laststatus=3"

--WARNING Why signcolumn dont work properly?
-- vim.api.nvim_create_autocmd("BufEnter", {
--   pattern = "*",
--   callback = function()
--     vim.opt.signcolumn = "number"
--   end
-- })

-- Git branch
-- local function branch_name()
--   local branch = ""
--   if vim.fn.has('win64') or vim.fn.has('win32') then
--     branch = vim.fn.system("git branch --show-current 2>&1 | grep -v 'fatal'"):match("[^\n]*")
--   elseif vim.fn.has('unix') then
--     branch = vim.fn.system("git branch --show-current 2> /dev/null | tr -d '\n'")
--   end
--   if branch ~= "" then
--     return " " .. branch
--   else
--     return ""
--   end
-- end
--
-- vim.api.nvim_create_autocmd({ "FileType", "BufEnter", "FocusGained" }, {
--   callback = function()
--     vim.b.branch_name = branch_name()
--   end
-- })

-- Show diagnostics under the cursor when holding position
-- vim.api.nvim_create_augroup("lsp_diagnostics_hold", { clear = true })
-- vim.api.nvim_create_autocmd({ "CursorHold" }, {
--   pattern = "*",
--   group = "lsp_diagnostics_hold",
--   command = "lua OpenDiagnosticIfNoFloat()",
-- })

-- Or, you can disable all semantic highlights by clearing all the groups
-- for _, group in ipairs(vim.fn.getcompletion('@lsp', 'highlight')) do
--     vim.api.nvim_set_hl(0, group, {})
-- end

--Toggle-checkbox Markdown
local checked_character = 'x'

local checked_checkbox = '%[' .. checked_character .. '%]'
local unchecked_checkbox = '%[ %]'

local line_contains_an_unchecked_checkbox = function(line)
    return string.find(line, unchecked_checkbox)
end

local checkbox = {
    check = function(line)
        return line:gsub(unchecked_checkbox, checked_checkbox)
    end,
    uncheck = function(line)
        return line:gsub(checked_checkbox, unchecked_checkbox)
    end,
}

local M = {}

M.toggle = function()
    local bufnr = vim.api.nvim_buf_get_number(0)
    local cursor = vim.api.nvim_win_get_cursor(0)
    local start_line = cursor[1] - 1
    local current_line = vim.api.nvim_buf_get_lines(bufnr, start_line, start_line + 1, false)[1] or ''

    -- If the line contains a checked checkbox then uncheck it.
    -- Otherwise, if it contains an unchecked checkbox, check it.
    local new_line = ''
    if line_contains_an_unchecked_checkbox(current_line) then
        new_line = checkbox.check(current_line)
    else
        new_line = checkbox.uncheck(current_line)
    end

    vim.api.nvim_buf_set_lines(bufnr, start_line, start_line + 1, false, { new_line })
    vim.api.nvim_buf_set_lines(bufnr, start_line, start_line + 1, false, { new_line })
    vim.api.nvim_win_set_cursor(0, cursor)
end

vim.api.nvim_create_user_command('ToggleCheckbox', M.toggle, {})

return M
