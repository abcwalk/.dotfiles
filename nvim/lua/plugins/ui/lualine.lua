local custom_auto = require('lualine.themes.auto')
local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
    return
end

local hide_in_width = function()
    return vim.fn.winwidth(0) > 80
end

local diagnostics = {
    'diagnostics',
    sources = { 'nvim_diagnostic' },
    sections = { 'error', 'warn' },
    symbols = { error = ' ', warn = ' ' },
    colored = false,
    update_in_insert = false,
    always_visible = true,
}

local diff = {
    'diff',
    colored = false,
    symbols = { added = ' ', modified = ' ', removed = ' ' },
    cond = hide_in_width,
}

local mode = {
    'mode',
    fmt = function(str)
        return '-- ' .. str .. ' --'
    end,
}

local function filename()
    return vim.fn.expand('%')
end

local filetype_no_icon = {
    'filetype',
    icons_enabled = false,
}

local filetype = {
    'filetype',
    colored = true,
    icon_only = true,
}

local branch = {
    'branch',
    icons_enabled = true,
    icon = '',
}

local location = {
    'location',
    padding = 0,
}

-- cool function for progress
local progress = function()
    local current_line = vim.fn.line('.')
    local total_lines = vim.fn.line('$')
    local chars = { '__', '▁▁', '▂▂', '▃▃', '▄▄', '▅▅', '▆▆', '▇▇', '██' }
    local line_ratio = current_line / total_lines
    local index = math.ceil(line_ratio * #chars)
    return chars[index]
end

local spaces = function()
    return 'spaces: ' .. vim.api.nvim_buf_get_option(0, 'shiftwidth')
end

local time = function()
    return os.date('%H:%M:%S')
end

local function is_recording()
    local reg = vim.fn.reg_recording()
    if reg == '' then
        return ''
    end -- not recording
    return '@' .. reg
end

custom_auto.normal.x = custom_auto.normal.c
custom_auto.insert.x = custom_auto.insert.c
custom_auto.visual.x = custom_auto.visual.c
custom_auto.replace.x = custom_auto.replace.c
custom_auto.command.x = custom_auto.command.c
custom_auto.inactive.x = custom_auto.inactive.c

lualine.setup({
    options = {
        icons_enabled = true,
        theme = custom_auto,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = { 'alpha', 'dashboard', 'NvimTree', 'Outline' },
        always_divide_middle = true,
    },
    sections = {
        lualine_a = { mode, is_recording },
        lualine_b = { branch, diagnostics, time },
        lualine_c = { '%=', filetype, filename, diff },
        lualine_x = { 'searchcount', 'selectioncount', spaces, 'encoding', filetype_no_icon },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
    },
    -- inactive_sections = {
    --   lualine_a = {},
    --   lualine_b = {},
    --   lualine_c = { "filename" },
    --   lualine_x = { "location" },
    --   lualine_y = {},
    --   lualine_z = {},
    -- },
    tabline = {},
    extensions = {},
})
