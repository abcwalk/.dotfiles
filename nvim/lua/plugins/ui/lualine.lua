-- Plugin configuration: lualine.nvim ------------------------------------------------------

local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
    return
end

--------------------------------------------------------------------------------------------
-- Local Functions                                                                        --
--------------------------------------------------------------------------------------------

local diagnostics = {
    'diagnostics',
    sources = { 'nvim_diagnostic' },
    sections = { 'error', 'warn' },
    -- symbols = { error = ' ', warn = ' ', hint = '' },
    symbols = { error = 'E', warn = 'W', hint = 'H' },
    colored = true,
    update_in_insert = true,
    always_visible = true,
    on_click = function()
        require('telescope.builtin').diagnostics()
    end,
}

local mode = {
    'mode',
    fmt = function(str)
        return '-- ' .. str .. ' --'
    end,
}

local filetype = {
    'filetype',
    icons_enabled = false,
    icon = nil,
}

local branch = {
    'branch',
    icons_enabled = true,
    icon = '',
    on_click = function()
        require('telescope.builtin').git_commits()
    end,
}

local filename = {
    'filename',
    on_click = function()
        local parent_dir = vim.fn.expand('%:p:h')
        vim.cmd('Oil ' .. parent_dir)
    end,
}

local time = function()
    return os.date('%H:%M:%S')
end

local swenv = {
    'swenv',
    icon = '󱔎',
    color = { fg = '#8cc85f' },
    on_click = function()
        require('swenv.api').pick_venv()
    end,
    cond = function()
        return vim.bo.filetype == 'python'
    end,
    fmt = function(env_name)
        local components = vim.split(env_name, vim.loop.os_homedir() == '\\' and '\\' or '/', { plain = true })

        if #components >= 2 then
            return components[#components - 1]
        else
            return 'venv'
        end
    end,
}

--------------------------------------------------------------------------------------------
-- Plugin Configuration                                                                   --
--------------------------------------------------------------------------------------------

-- branch (git branch)
-- buffers (shows currently available buffers)
-- diagnostics (diagnostics count from your preferred source)
-- diff (git diff status)
-- encoding (file encoding)
-- fileformat (file format)
-- filename
-- filesize
-- filetype
-- hostname
-- location (location in file in line:column format)
-- mode (vim mode)
-- progress (%progress in file)
-- searchcount (number of search matches when hlsearch is active)
-- selectioncount (number of selected characters or lines)
-- tabs (shows currently available tabs)
-- windows (shows currently available windows)

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
        lualine_b = { branch, swenv },
        lualine_c = { diagnostics, '%=', filename },
        lualine_x = {},
        lualine_y = { time },
        lualine_z = { filetype },
    },

    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { filename },
        lualine_x = {},
        lualine_y = {},
        lualine_z = { filetype },
    },
    tabline = {},
    extensions = {},
})
