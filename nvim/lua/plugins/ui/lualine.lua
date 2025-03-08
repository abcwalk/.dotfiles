local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
    return
end

-- LSP clients attached to buffer
local clients_lsp = function()
    local bufnr = vim.api.nvim_get_current_buf()

    local clients = vim.lsp.buf_get_clients(bufnr)
    if next(clients) == nil then
        return ''
    end

    local c = {}
    for _, client in pairs(clients) do
        table.insert(c, client.name)
    end
    return '[' .. table.concat(c, '|') .. ']' -- \u{f085}
end

lualine.setup({
    options = {
        globalstatus = true,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        always_divide_middle = true,
    },
    sections = {
        lualine_a = {},
        lualine_b = {
            {
                'branch',
                icons_enabled = true,
                icon = '',
                color = { bg = '#162022', fg = '#9f9f9f' },
            },
        },
        lualine_c = {
            {
                'filename',
                path = 4,
                color = { bg = '#162022', fg = '#696969' },
            },
        },
        lualine_x = {
            { 'lsp_progress' },
            { 'diagnostics' },
        },
        lualine_y = {
            {
                'location',
                color = { bg = '#162022', fg = '#9f9f9f' },
            },
            {
                clients_lsp,
                color = { bg = '#162022', fg = '#696969' },
            },
        },
        lualine_z = {},
    },
})
