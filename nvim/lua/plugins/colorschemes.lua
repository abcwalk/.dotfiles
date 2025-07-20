local uv = vim.uv
local THEME = 'cyberdream'
local theme_file_path = vim.fn.expand('$HOME/.theme')

local themes = {
    ['alabaster'] = {
        repo = 'behemothbucket/alabaster.nvim',
        branch = 'custom',
        config = function()
            vim.g.alabaster_dim_comments = true
            vim.g.alabaster_floatborder = true
        end,
    },
    ['gruvbox-material'] = {
        repo = 'sainnhe/gruvbox-material',
        config = function()
            vim.g.gruvbox_material_transparent_background = 0
            vim.g.gruvbox_material_foreground = 'mix'
            vim.g.gruvbox_material_background = 'hard'
            vim.g.gruvbox_material_ui_contrast = 'high'
            vim.g.gruvbox_material_float_style = 'bright'
            vim.g.gruvbox_material_statusline_style = 'material'
            vim.g.gruvbox_material_cursor = 'auto'
        end,
    },
    ['cyberdream'] = {
        repo = 'scottmckendry/cyberdream.nvim',
        config = function()
            require('cyberdream').setup({
                saturation = 0.90,
                highlights = {
                    Search = { bg = '#3c4047' },
                },
                colors = {
                    dark = {
                        bg = '#0e1415',
                    },
                },
            })
        end,
    },
    ['naysayer'] = {
        repo = 'RostislavArts/naysayer.nvim',
        config = function() end,
    },
}

local function read_mode_from_file()
    local file = io.open(theme_file_path, 'r')
    if not file then
        return 'dark'
    end
    local content = file:read('*line')
    file:close()
    return content:lower()
end

local function set_colorscheme()
    local mode = read_mode_from_file()

    if mode == 'light' then
        vim.api.nvim_set_option_value('background', 'light', {})
    else
        vim.api.nvim_set_option_value('background', 'dark', {})
    end

    vim.cmd('colorscheme ' .. THEME)
end

local function watch_theme_change()
    local handle = uv.new_fs_event()

    local unwatch_cb = function()
        if handle then
            uv.fs_event_stop(handle)
        end
    end

    local event_cb = function(err)
        if err then
            vim.notify('Theme file watcher failed', vim.log.levels.ERROR)
            unwatch_cb()
        else
            vim.schedule(function()
                set_colorscheme()
                vim.cmd('Lazy reload tiny-glimmer.nvim')
                unwatch_cb()
                watch_theme_change()
            end)
        end
    end

    local flags = {
        watch_entry = false,
        stat = false,
        recursive = false,
    }

    if handle then
        uv.fs_event_start(handle, theme_file_path, flags, event_cb)
    end

    return handle
end

watch_theme_change()

local plugins = {}

for name, opts in pairs(themes) do
    table.insert(plugins, {
        opts.repo,
        branch = opts.branch or nil,
        config = function()
            opts.config()
            if name == THEME then
                set_colorscheme()
            end
        end,
    })
end

return plugins
