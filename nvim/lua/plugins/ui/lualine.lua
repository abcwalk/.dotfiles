local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
    return
end

-- local kanagawa_paper = require('lualine.themes.kanagawa-paper')

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
    return '\u{f085}  ' .. table.concat(c, '|')
end

lualine.setup({
    options = {
        globalstatus = true,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        always_divide_middle = true,
        -- theme = kanagawa_paper,
    },
    sections = {
        lualine_a = {
            { -- Project abbreviation
                function()
                    local projectName = vim.fs.basename(vim.fn.getcwd())
                    local firstChars = {}
                    for str in string.gmatch(projectName, '([^-_,%s.]+)') do
                        table.insert(firstChars, string.upper(string.sub(str, 1, 1)))
                    end
                    return (firstChars[1] or '')
                        .. (
                            #firstChars > 1 and firstChars[#firstChars]
                            or string.upper(string.sub(projectName, 2, 2))
                            or ''
                        )
                end,
            },
        },
        lualine_b = {
            { -- Project name
                function()
                    local cwd = vim.fn.getcwd()
                    local devpath = vim.fn.fnamemodify('~/Developer', ':p')

                    if cwd:find(devpath, 1, true) == 1 then
                        local name = vim.fs.basename(cwd)
                        local code = vim.fs.basename(vim.fs.dirname(cwd))
                        local account = vim.fs.basename(vim.fs.dirname(vim.fn.fnamemodify(cwd, ':h')))

                        return account .. '' .. (tonumber(code) or code) .. ' ' .. name
                    else
                        return vim.fs.basename(cwd)
                    end
                end,
            },
            {
                'branch',
                icons_enabled = true,
                icon = '',
                on_click = function()
                    vim.cmd('Neogit')
                end,
            },
            -- "diff",
        },
        lualine_c = {
            {
                'filetype',
                padding = { left = 1, right = 0 },
                icon_only = true,
            },
            {
                'filename',
                file_status = true,
                newfile_status = true,
                path = 1, -- Show relative path
                symbols = { newfile = '[new]', unnamed = '[no name]' },
                fmt = function(name, _)
                    local filePath, rest = name:match('(.+)%s*(%[*.*%]*)')
                    local parentPath = vim.fn.fnamemodify(filePath, ':h')
                    local fileName = vim.fs.basename(filePath)

                    if string.match(name, 'term://.*toggleterm#.*') then
                        local terms = require('toggleterm.terminal').get_all()
                        local term = require('toggleterm.terminal').get(tonumber(vim.b.toggle_number))
                        local termid = term and term.id or ''
                        local termname = term and termid .. ': ' .. (term:_display_name()) or ''
                        return 'term ' .. termname .. ' (' .. #terms .. ') ' .. (rest or '')
                    end
                    if string.match(name, 'term://.*') then
                        local path_parts = vim.fn.split(vim.fn.expand('%'), ':')
                        local last = path_parts[#path_parts]
                        if type(last) == 'string' and last ~= '' then
                            return last and 'term ' .. last
                        end
                    end

                    local shorten_after = math.floor(vim.o.columns / 238 * 70)
                    if string.len(filePath) > shorten_after then
                        local rightPart = vim.fs.basename(parentPath) .. '/' .. fileName
                        local leftPart = string.sub(filePath, 1, shorten_after - string.len(rightPart))
                        return leftPart .. '../' .. rightPart .. ' ' .. (rest or '')
                    else
                        return filePath .. ' ' .. (rest or '')
                    end
                end,
                -- color = function(_)
                --     return vim.b.custom_git_status_hl or 'Custom_TabSel'
                -- end,
                on_click = function()
                    local parent_dir = vim.fn.expand('%:p:h')
                    vim.cmd('Oil ' .. parent_dir)
                end,
            },
            {
                'navic',

                -- Component specific options
                color_correction = 'dynamic', -- Can be nil, "static" or "dynamic". This option is useful only when you have highlights enabled.
                -- Many colorschemes don't define same backgroud for nvim-navic as their lualine statusline backgroud.
                -- Setting it to "static" will perform a adjustment once when the component is being setup. This should
                --   be enough when the lualine section isn't changing colors based on the mode.
                -- Setting it to "dynamic" will keep updating the highlights according to the current modes colors for
                --   the current section.

                navic_opts = {
                    click = true,
                    separator = '  ',
                    highlight = true,
                }, -- lua table with same format as setup's option. All options except "lsp" options take effect when set here.
            },
        },
        lualine_x = {
            { 'lsp_progress' },
            { 'copilot' },
            { 'diagnostics' },
            {
                'overseer',
                label = '', -- Prefix for task counts
                colored = true, -- Color the task icons and counts
                unique = false, -- Unique-ify non-running task count by name
                name = nil, -- List of task names to search for
                name_not = false, -- When true, invert the name search
                status = nil, -- List of task statuses to display
                status_not = false, -- When true, invert the status search
            },
        },
        lualine_y = {
            'searchcount',
            'progress',
            'location',
        },
        lualine_z = {
            {
                clients_lsp,
                on_click = function()
                    vim.cmd('LspInfo')
                end,
            },
            --         {
            --             'mode',
            --             fmt = function(mode)
            --                 local modes = {
            --                     ['NORMAL'] = 'NORM',
            --                     ['O-PENDING'] = 'OPND',
            --                     ['VISUAL'] = 'VISU',
            --                     ['V-LINE'] = 'VISL',
            --                     ['V-BLOCK'] = 'VISB',
            --                     ['SELECT'] = 'SELE',
            --                     ['S-LINE'] = 'SELL',
            --                     ['S-BLOCK'] = 'SELB',
            --                     ['INSERT'] = 'INSE',
            --                     ['REPLACE'] = 'RPLC',
            --                     ['V-REPLACE'] = 'VRPL',
            --                     ['COMMAND'] = 'COMM',
            --                     ['EX'] = 'ExEC',
            --                     ['MORE'] = 'MORE',
            --                     ['CONFIRM'] = 'CONF',
            --                     ['SHELL'] = 'SHEL',
            --                     ['TERMINAL'] = 'TERM',
            --                 }
            --                 return modes[mode] or mode
            --             end,
            --         },
        },
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
    },
})

-- -- Plugin configuration: lualine.nvim ------------------------------------------------------
--
-- local status_ok, lualine = pcall(require, 'lualine')
-- if not status_ok then
--     return
-- end
--
-- --------------------------------------------------------------------------------------------
-- -- Local Functions                                                                        --
-- --------------------------------------------------------------------------------------------
--
-- local diagnostics = {
--     'diagnostics',
--     sources = { 'nvim_diagnostic' },
--     sections = { 'error', 'warn' },
--     -- symbols = { error = ' ', warn = ' ', hint = '' },
--     symbols = { error = 'E', warn = 'W', hint = 'H' },
--     colored = true,
--     update_in_insert = true,
--     always_visible = true,
--     on_click = function()
--         require('telescope.builtin').diagnostics()
--     end,
-- }
--
-- local mode = {
--     'mode',
--     fmt = function(str)
--         return '-- ' .. str .. ' --'
--     end,
-- }
--
-- local filetype = {
--     'filetype',
--     icons_enabled = false,
--     icon = nil,
-- }
--
-- local branch = {
--     'branch',
--     icons_enabled = true,
--     icon = '',
--     on_click = function()
--         require('telescope.builtin').git_commits()
--     end,
-- }
--
-- local filename = {
--     'filename',
--     file_status = true,
--     newfile_status = false,
--     path = 1,
--     on_click = function()
--         local parent_dir = vim.fn.expand('%:p:h')
--         vim.cmd('Oil ' .. parent_dir)
--     end,
-- }
--
-- local time = function()
--     return os.date('%H:%M:%S')
-- end
--
-- local swenv = {
--     'swenv',
--     icon = '󱔎',
--     color = { fg = '#8cc85f' },
--     on_click = function()
--         require('swenv.api').pick_venv()
--     end,
--     cond = function()
--         return vim.bo.filetype == 'python'
--     end,
--     fmt = function(env_name)
--         local components = vim.split(env_name, vim.loop.os_homedir() == '\\' and '\\' or '/', { plain = true })
--
--         if #components >= 2 then
--             return components[#components - 1]
--         else
--             return 'venv'
--         end
--     end,
-- }
--
-- --------------------------------------------------------------------------------------------
-- -- Plugin Configuration                                                                   --
-- --------------------------------------------------------------------------------------------
--
-- -- branch (git branch)
-- -- buffers (shows currently available buffers)
-- -- diagnostics (diagnostics count from your preferred source)
-- -- diff (git diff status)
-- -- encoding (file encoding)
-- -- fileformat (file format)
-- -- filename
-- -- filesize
-- -- filetype
-- -- hostname
-- -- location (location in file in line:column format)
-- -- mode (vim mode)
-- -- progress (%progress in file)
-- -- searchcount (number of search matches when hlsearch is active)
-- -- selectioncount (number of selected characters or lines)
-- -- tabs (shows currently available tabs)
-- -- windows (shows currently available windows)
--
-- lualine.setup({
--     options = {
--
--         disabled_filetypes = {
--             'alpha',
--             'dashboard',
--             'neo-tree',
--             'noice',
--             'starter',
--         },
--
--         icons_enabled = true,
--         component_separators = { left = '', right = '' },
--         section_separators = { left = '', right = '' },
--         always_divide_middle = false,
--         theme = 'moonfly',
--     },
--
--     sections = {
--         lualine_a = { branch },
--         lualine_b = { filename },
--         lualine_c = { diagnostics },
--         lualine_x = { swenv },
--         lualine_y = {},
--         lualine_z = { filetype },
--     },
--
--     inactive_sections = {
--         lualine_a = {},
--         lualine_b = { filename },
--         lualine_c = {},
--         lualine_x = {},
--         lualine_y = {},
--         lualine_z = { filetype },
--     },
--
--     tabline = {},
--     extensions = {},
-- })
