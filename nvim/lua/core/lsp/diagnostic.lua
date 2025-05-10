local M = {}

M.signs = { Error = '✘', Warn = '', Hint = '', Info = '', Question = '' }

function M.setup()
    local signs = {
        { name = 'DiagnosticSignError', text = '' },
        { name = 'DiagnosticSignWarn', text = '' },
        { name = 'DiagnosticSignHint', text = '' },
        { name = 'DiagnosticSignInfo', text = '' },
    }

    vim.diagnostic.config({
        underline = true,
        update_in_insert = true,
        severity_sort = true,
        virtual_lines = false,
        signs = {
            active = signs,
        },
    })

    local function disable_virtual_lines()
        vim.diagnostic.config({
            virtual_lines = false,
        })
    end

    vim.api.nvim_create_autocmd('LspAttach', {
        pattern = '*',
        callback = disable_virtual_lines,
    })
end

return M
