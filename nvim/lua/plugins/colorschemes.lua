local uv = vim.uv
local theme_file_path = vim.fn.expand('$HOME/.theme')
local current_theme = ''

local themes = {
    ['alabaster'] = {
        repo = 'behemothbucket/alabaster.nvim',
        branch = 'custom',
        enabled = true,
        config = function()
            vim.g.alabaster_dim_comments = true
            vim.g.alabaster_floatborder = true
        end,
    },
    ['gruvbox-material'] = {
        repo = 'sainnhe/gruvbox-material',
        enabled = false,
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
        enabled = false,
        config = function()
            require('cyberdream').setup({
                -- saturation = 0.7,
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
        repo = 'abcwalk/naysayer.nvim',
        enabled = false,
        config = function() end,
    },
    ['cobalt'] = {
        repo = 'wurli/cobalt.nvim',
        enabled = false,
        config = function()
            require('cobalt').setup({
                commentStyle = { italic = false },
                keywordStyle = { italic = false },
                overrides = function(colors)
                    return {
                        NormalFloat = { bg = 'none' },
                        FloatBorder = { bg = 'none' },
                        FloatTitle = { bg = 'none' },
                        Function = { fg = colors.palette.AshenGrey },
                        ['@variable.builtin'] = { italic = false },

                        BlinkCmpMenuBorder = { link = 'FloatBorder' },
                        NoiceCmdlinePopupBorder = { link = 'FloatBorder' },
                        DiagnosticSignInfo = { bg = 'none' },
                        DiagnosticSignWarn = { bg = 'none' },
                        DiagnosticSignError = { link = 'Constant' },
                        DiagnosticFloatingError = { link = 'Constant' },
                        DiagnosticFloatingWarn = { link = '@keyword.return' },
                        DiagnosticUnderlineError = { sp = colors.palette.BlushPink },
                        DiagnosticUnderlineWarn = { sp = colors.palette.PeachSherbet },
                    }
                end,
            })
        end,
    },
    ['ef-theme'] = {
        repo = 'oonamo/ef-themes.nvim',
        enabled = false,
        config = function()
            require('ef-themes').setup({
                light = 'ef-eagle',
                dark = 'ef-dream',
                styles = {
                    comments = { italic = true },
                },
                on_highlights = function(hls, palette, name)
                    local overrides = {
                        NormalFloat = { fg = palette.fg_main, bg = 'NONE' },
                        -- FloatBorder = { fg = palette.fg_main, bg = 'NONE' },
                        -- BlinkCmpMenuBorder = { link = 'FloatBorder' },
                        -- NoiceCmdlinePopupBorder = { link = 'FloatBorder' },
                    }

                    return overrides
                end,
            })
        end,
    },
    ['silkcircuit'] = {
        repo = 'hyperb1iss/silkcircuit-nvim',
        enabled = false,
        config = function()
            require('silkcircuit').setup({
                variant = 'soft', -- "neon" | "vibrant" | "soft" | "glow"
                on_highlights = function(highlights, colors)
                    highlights.FloatBorder = { bg = colors.none }
                    highlights.Pmenu = { bg = colors.none }
                    highlights.DiagnosticSignHint = { bg = colors.none }
                end,
            })
        end,
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

    vim.cmd('colorscheme ' .. current_theme)
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
            if opts.enabled then
                current_theme = name
                set_colorscheme()
            end
        end,
    })
end

return plugins
