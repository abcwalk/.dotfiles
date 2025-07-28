return {
    'folke/snacks.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        bigfile = { enabled = true },
        explorer = {
            enabled = true,
            auto_close = true,
            jump = { close = true },
        },
        image = { enabled = false },
        input = {
            enabled = true,
            styles = {
                input = {
                    b = {
                        completion = false, -- disable blink completions in input
                    },
                },
            },
        },
        notifier = { enabled = true },
        picker = {
            enabled = true,
            hidden = true,
            ignored = true,
            formatters = {
                file = {
                    truncate = 100,
                },
            },
            sources = {
                smart = {
                    exclude = { 'framework/build' },
                    layout = {
                        cycle = true,
                        preset = 'vertical',
                        layout = {
                            backdrop = true,
                            width = 0.6,
                            min_width = 140,
                            height = 0.8,
                            min_height = 30,
                            box = 'vertical',
                            border = 'rounded',
                            title = '{title} {live} {flags}',
                            title_pos = 'left',
                            { win = 'input', height = 1, border = 'bottom' },
                            { win = 'list', border = 'none' },
                            { win = 'preview', title = nil, height = 0.6, border = 'top' },
                        },
                    },
                    filter = {
                        paths = {
                            ['*build*'] = false,
                        },
                    },
                },
                grep = {
                    exclude = { 'framework/build' },
                    layout = {
                        cycle = true,
                        preset = 'vertical',
                        layout = {
                            backdrop = true,
                            width = 0.6,
                            min_width = 140,
                            height = 0.8,
                            min_height = 30,
                            box = 'vertical',
                            border = 'rounded',
                            title = '{title} {live} {flags}',
                            title_pos = 'left',
                            { win = 'input', height = 1, border = 'bottom' },
                            { win = 'list', border = 'none' },
                            { win = 'preview', title = nil, height = 0.6, border = 'top' },
                        },
                    },
                },
                git_diff = {
                    layout = {
                        cycle = true,
                        preset = 'sidebar',
                    },
                },
                recent = {
                    layout = {
                        cycle = true,
                        preset = 'vertical',
                        layout = {
                            backdrop = true,
                            width = 0.6,
                            min_width = 140,
                            height = 0.8,
                            min_height = 30,
                            box = 'vertical',
                            border = 'rounded',
                            title = '{title} {live} {flags}',
                            title_pos = 'left',
                            { win = 'input', height = 1, border = 'bottom' },
                            { win = 'list', border = 'none' },
                            { win = 'preview', title = nil, height = 0.6, border = 'top' },
                        },
                    },
                },
            },
        },
        quickfile = { enabled = true },
        scroll = { enabled = false },
        statuscolumn = { enabled = true },
        words = { enabled = true },
    },
    keys = {
        {
            '<leader><space>',
            function()
                Snacks.picker.smart()
            end,
            desc = 'Smart Find Files',
        },
        {
            '<C-e>',
            function()
                Snacks.picker.recent()
            end,
            desc = 'Recent',
        },
        {
            '<Bslash><Bslash>',
            function()
                Snacks.picker.grep({
                    -- exclude = { 'framework/build' },
                })
            end,
            desc = 'Grep',
        },
        {
            '<C-p>',
            function()
                Snacks.picker.projects()
            end,
            desc = 'Projects',
        },
        {
            '<leader>e',
            function()
                Snacks.explorer()
            end,
            desc = 'File Explorer',
        },
        {
            '<leader>o',
            function()
                Snacks.explorer.reveal()
            end,
            desc = 'File Explorer Reveal',
        },

        -- find
        {
            '<leader>b',
            function()
                Snacks.picker.buffers({
                    win = {
                        input = {
                            keys = {
                                ['dd'] = 'bufdelete',
                                ['<c-d>'] = { 'bufdelete', mode = { 'n', 'i' } },
                            },
                        },
                        list = { keys = { ['dd'] = 'bufdelete' } },
                    },
                })
            end,
            desc = 'Buffers',
        },
        {
            '<F3>',
            function()
                Snacks.picker.files({ cwd = vim.fn.stdpath('config') })
            end,
            desc = 'Find Nvim configs',
        },
        {
            '<F4>',
            function()
                Snacks.picker.files({ cwd = os.getenv('HOME') .. '/.config' })
            end,
            desc = 'Find Dotfiles configs',
        },
        -- Grep
        {
            '<leader>sw',
            function()
                Snacks.picker.grep_word()
            end,
            desc = 'Visual selection or word',
            mode = { 'n', 'x' },
        },
        -- search
        {
            '<C-f>',
            function()
                Snacks.picker.lines()
            end,
            desc = 'Buffer Lines',
        },
        {
            '<leader>sd',
            function()
                Snacks.picker.diagnostics()
            end,
            desc = 'Diagnostics',
        },
        {
            '<leader>sH',
            function()
                Snacks.picker.highlights()
            end,
            desc = 'Highlights',
        },
        {
            '<leader>si',
            function()
                Snacks.picker.icons()
            end,
            desc = 'Icons',
        },
        {
            '<leader>sk',
            function()
                Snacks.picker.keymaps()
            end,
            desc = 'Keymaps',
        },
        {
            '<leader>sl',
            function()
                Snacks.picker.loclist()
            end,
            desc = 'Location List',
        },
        {
            '<leader>sm',
            function()
                Snacks.picker.marks()
            end,
            desc = 'Marks',
        },
        {
            '<leader>sp',
            function()
                Snacks.picker.lazy()
            end,
            desc = 'Search for Plugin Spec',
        },
        {
            '<leader>sq',
            function()
                Snacks.picker.qflist()
            end,
            desc = 'Quickfix List',
        },
        {
            '<leader>sR',
            function()
                Snacks.picker.resume()
            end,
            desc = 'Resume',
        },
        {
            '<leader>su',
            function()
                Snacks.picker.undo()
            end,
            desc = 'Undo History',
        },
        {
            '<leader>uC',
            function()
                Snacks.picker.colorschemes()
            end,
            desc = 'Colorschemes',
        },
        -- Other
        {
            '<leader>z',
            function()
                Snacks.zen()
            end,
            desc = 'Toggle Zen Mode',
        },
        {
            '<leader>cR',
            function()
                Snacks.rename.rename_file()
            end,
            desc = 'Rename File',
        },
        {
            '<leader>gB',
            function()
                Snacks.gitbrowse()
            end,
            desc = 'Git Browse',
            mode = { 'n', 'v' },
        },
        {
            '<leader>gd',
            function()
                Snacks.picker.git_diff()
            end,
            desc = 'Git Diff',
            mode = { 'n', 'v' },
        },
        {
            '<leader>gS',
            function()
                Snacks.picker.git_branches()
            end,
            desc = 'Git Branches',
            mode = { 'n', 'v' },
        },

        {
            '<leader><Bslash>',
            function()
                Snacks.terminal()
            end,
            desc = 'Toggle Terminal',
        },
    },
    init = function()
        vim.api.nvim_create_autocmd('User', {
            pattern = 'VeryLazy',
            callback = function()
                -- Setup some globals for debugging (lazy-loaded)
                _G.dd = function(...)
                    Snacks.debug.inspect(...)
                end
                _G.bt = function()
                    Snacks.debug.backtrace()
                end
                vim.print = _G.dd -- Override print to use snacks for `:=` command

                -- Create some toggle mappings
                Snacks.toggle.option('spell', { name = 'Spelling' }):map('<leader>us')
                Snacks.toggle.option('wrap', { name = 'Wrap' }):map('<leader>uw')
                Snacks.toggle.diagnostics():map('<leader>ud')
                Snacks.toggle.line_number():map('<leader>ul')
                Snacks.toggle
                    .option('conceallevel', { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2 })
                    :map('<leader>uc')
                Snacks.toggle.treesitter():map('<leader>uT')
                Snacks.toggle
                    .option('background', { off = 'light', on = 'dark', name = 'Dark Background' })
                    :map('<leader>ub')
                Snacks.toggle.inlay_hints():map('<leader>uh')
                Snacks.toggle.indent():map('<leader>ug')
                Snacks.toggle.dim():map('<leader>uD')
            end,
        })
    end,
}
