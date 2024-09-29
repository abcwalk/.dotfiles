-- Plugin configuration: lualine.nvim ------------------------------------------------------

local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
    return
end

--------------------------------------------------------------------------------------------
-- Local Functions                                                                        --
--------------------------------------------------------------------------------------------

-- display the number of errors, warnings, and infos
local diagnostics = {
    'diagnostics',
    sources = { 'nvim_diagnostic' },
    sections = { 'error', 'warn', 'hint' },
    symbols = { error = 'E', warn = 'W', hint = 'H' },
    colored = false,
    update_in_insert = false,
    always_visible = true,
}

local diagnostics = {
    'diagnostics',
    sources = { 'nvim_diagnostic' },
    sections = { 'error', 'warn' },
    symbols = { error = ' ', warn = ' ', hint = '' },
    colored = true,
    update_in_insert = true,
    always_visible = false,
}

-- display the current mode
local mode = {
    'mode',
    fmt = function(str)
        return '-- ' .. str .. ' --'
    end,
}

-- display the active filetype
local filetype = {
    'filetype',
    icons_enabled = false,
    icon = nil,
}

-- display the git branch
local branch = {
    'branch',
    icons_enabled = true,
    icon = '',
}

--------------------------------------------------------------------------------------------
-- Plugin Configuration                                                                   --
--------------------------------------------------------------------------------------------

lualine.setup({
    options = {
        -- Filetypes in which we will hide the bar.
        disabled_filetypes = {
            'alpha',
            'dashboard',
            'neo-tree',
            'noice',
            'starter',
        },
        icons_enabled = true,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        always_divide_middle = false,
        theme = 'moonfly',
    },

    sections = {
        lualine_a = { mode },
        lualine_b = { branch },
        lualine_c = { diagnostics },
        lualine_x = {},
        lualine_y = {},
        lualine_z = { filetype },
    },

    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = { filetype },
    },
    tabline = {},
    extensions = {},
})
