-- Plugin configuration: lualine.nvim ------------------------------------------------------

local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
    return
end

--------------------------------------------------------------------------------------------
-- Local Variables                                                                        --
--------------------------------------------------------------------------------------------

-- Table to map the current mode into characters to be shown at the bar.
local vim_mode_map = {
    ['n'] = ' N ',
    ['no'] = ' O ',
    ['nov'] = ' O ',
    ['noV'] = ' O ',
    ['no\22'] = ' O ',
    ['niI'] = ' N ',
    ['niR'] = ' N ',
    ['niV'] = ' N ',
    ['nt'] = ' N ',
    ['ntT'] = ' N ',
    ['v'] = ' V ',
    ['vs'] = ' V ',
    ['V'] = 'VL ',
    ['Vs'] = 'VL ',
    ['\22'] = 'VB ',
    ['\22s'] = 'VB ',
    ['s'] = ' S ',
    ['S'] = 'SL ',
    ['\19'] = 'SB ',
    ['i'] = ' I ',
    ['ic'] = ' I ',
    ['ix'] = ' I ',
    ['R'] = ' R ',
    ['Rc'] = ' R ',
    ['Rx'] = ' R ',
    ['Rv'] = 'VR ',
    ['Rvc'] = 'VR ',
    ['Rvx'] = 'VR ',
    ['c'] = 'CMD',
    ['cv'] = 'EX ',
    ['ce'] = 'EX ',
    ['r'] = ' R ',
    ['rm'] = ' M ',
    ['r?'] = ' C ',
    ['!'] = ' S ',
    ['t'] = ' T ',
}

--------------------------------------------------------------------------------------------
-- Local Functions                                                                        --
--------------------------------------------------------------------------------------------

-- Return the lines and characters selected by the visual mode.
local function get_visual_selection_information()
    local is_visual_mode = vim.fn.mode():find('[Vv]')
    if not is_visual_mode then
        return ''
    end

    local start_line = vim.fn.line('v')
    local end_line = vim.fn.line('.')
    local lines = start_line <= end_line and end_line - start_line + 1 or start_line - end_line + 1

    return '[' .. tostring(lines) .. 'L ' .. tostring(vim.fn.wordcount().visual_chars) .. 'C]'
end

-- Return the current mode.
local function get_vim_mode_text()
    local mode_code = vim.api.nvim_get_mode().mode
    if vim_mode_map[mode_code] == nil then
        return mode_code
    end

    return vim_mode_map[mode_code]
end

-- Show the macro recording information.
local function show_macro_recording()
    local recording_register = vim.fn.reg_recording()
    if recording_register == '' then
        return ''
    else
        return '● ' .. recording_register
    end
end

-- Show the tab indicator.
local function show_tab_indicator()
    local num_tabs = vim.fn.tabpagenr('$')

    if num_tabs <= 1 then
        return ''
    end

    local current_tab = vim.fn.tabpagenr()
    return '[' .. tostring(current_tab) .. ' / ' .. tostring(num_tabs) .. ']'
end

--------------------------------------------------------------------------------------------
-- Custom Components
--------------------------------------------------------------------------------------------

local env_stat = function()
    local swenv = require('swenv.api')
    local current_env = '[-]' -- No environment

    if swenv.get_current_venv() ~= nil then
        -- Environment loaded by swenv
        local _name = swenv.get_current_venv().name
        local _src = swenv.get_current_venv().source
        current_env = _name .. ' (' .. _src .. ')'
    elseif vim.g.python3_host_prog ~= nil then
        -- Default environment from python3_host_prog
        local Path = require('plenary.path')
        local tokens = Path._split(Path:new(vim.g.python3_host_prog))

        -- Get the environment name from python3_host_prog
        if #tokens > 2 then
            -- Standard path is .../[name]/bin/python, so get the third last token
            current_env = tokens[#tokens - 2]

            -- Check if python3_host_prog is registered in swenv and get its source
            for _, v in ipairs(swenv.get_venvs()) do
                if v.name == current_env then
                    current_env = current_env .. ' (' .. v.source .. ')'
                    break
                end
            end
        end
    end

    return current_env
end

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
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        theme = 'oxocharcoal',
    },

    -- The status bar will show only the buffer list.
    sections = {
        lualine_a = {},
        lualine_b = {
            {
                'buffers',
                -- buffers_color = {
                --     active = function()
                --         local c = require('nano-theme.colors').get()
                --         return { fg = c.nano_salient_color, gui = 'bold' }
                --     end,
                --
                --     inactive = function()
                --         local c = require('nano-theme.colors').get()
                --         return { fg = c.nano_foreground_color }
                --     end,
                -- },
                mode = 0,
                symbols = {
                    alternate_file = '',
                    modified = ' [+]',
                },
            },
        },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
    },

    -- Set the winbar sections.
    winbar = {
        lualine_a = {
            {
                'vim_mode',
                fmt = get_vim_mode_text,
            },
        },

        lualine_b = {
            { 'macro', fmt = show_macro_recording },
            { 'filetype', color = { gui = 'bold' } },
            { 'filename' },
            {
                'diagnostics',
                on_click = function()
                    require('telescope.builtin').diagnostics()
                end,
            },
        },

        lualine_c = {
            {
                'branch',
                -- color = function()
                --     local c = require('nano-theme.colors').get()
                --     return { fg = c.nano_faded_color }
                -- end,
                on_click = function()
                    require('telescope.builtin').git_branches()
                end,
            },
            {
                env_stat,
                icon = '󱔎',
                color = { fg = '#8fb55e' },
                on_click = function()
                    require('swenv.api').pick_venv()
                end,
                cond = function()
                    return vim.bo.filetype == 'python'
                end,
            },
        },

        lualine_x = {
            {
                'visual_selection',
                fmt = get_visual_selection_information,
            },
        },

        lualine_y = {
            -- This component is necessary to allow Neovide to keep the winbar fixed while
            -- scrolling with smooth scroll enabled.
            -- {
            --     'spacer',
            --     color = 'WinBar',
            --     fmt = function()
            --         return ' '
            --     end,
            -- },
            {
                'progress',
            },
            {
                'location',
            },
            {
                'current_tab',
                fmt = show_tab_indicator,
            },
        },

        lualine_z = {},
    },

    inactive_winbar = {
        lualine_a = {
            {
                'inactive_mode',
                fmt = function()
                    return '   '
                end,
            },
        },

        lualine_b = {
            { 'filetype', color = { gui = 'bold' } },
            { 'filename' },
            {
                'diagnostics',
                on_click = function()
                    require('telescope.builtin').diagnostics()
                end,
            },
        },

        lualine_c = {
            {
                'branch',
                on_click = function()
                    require('telescope.builtin').git_branches()
                end,
            },

            {
                env_stat,
                icon = '󱔎',
                color = { fg = '#8fb55e' },
                on_click = function()
                    require('swenv.api').pick_venv()
                end,
                cond = function()
                    return vim.bo.filetype == 'python'
                end,
            },
        },

        lualine_x = {},

        lualine_y = {},

        lualine_z = {
            -- This component is necessary to allow Neovide to keep the winbar fixed while
            -- scrolling with smooth scroll enabled.
            {
                'spacer',
                color = 'WinBar',
                fmt = function()
                    return ' '
                end,
            },
        },
    },
})
