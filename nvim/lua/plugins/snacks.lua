---@diagnostic disable: undefined-global
return {
    'folke/snacks.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        bigfile = { enabled = true },
        -- dashboard = { enabled = true },
        explorer = { enabled = true },
        image = { enabled = true },
        input = { enabled = true },
        picker = {
            hidden = true,
            ignored = true,
            enabled = true,
            sources = {
                files = { hidden = true },
            },
        },
        quickfile = { enabled = true },
        scroll = { enabled = false },
        statuscolumn = { enabled = true },
        words = { enabled = true },
        styles = {
            input = {
                relative = 'cursor',
                b = {
                    completion = false, -- disable blink completions in input
                },
            },
        },
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
                Snacks.picker.grep()
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
            '<leader>sf',
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
            desc = 'Find Config File',
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
            '<leader>sM',
            function()
                Snacks.picker.man()
            end,
            desc = 'Man Pages',
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
        -- LSP
        -- Other
        {
            '<leader>z',
            function()
                Snacks.zen()
            end,
            desc = 'Toggle Zen Mode',
        },
        {
            '<leader>Z',
            function()
                Snacks.zen.zoom()
            end,
            desc = 'Toggle Zoom',
        },
        {
            '<leader>n',
            function()
                Snacks.notifier.show_history()
            end,
            desc = 'Notification History',
        },
        {
            'Q',
            function()
                Snacks.bufdelete()
            end,
            desc = 'Delete Buffer',
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
            '<leader>un',
            function()
                Snacks.notifier.hide()
            end,
            desc = 'Dismiss All Notifications',
        },
        {
            '<c-/>',
            function()
                Snacks.terminal()
            end,
            desc = 'Toggle Terminal',
        },
        {
            '<c-_>',
            function()
                Snacks.terminal()
            end,
            desc = 'which_key_ignore',
        },
        {
            ']]',
            function()
                Snacks.words.jump(vim.v.count1)
            end,
            desc = 'Next Reference',
            mode = { 'n', 't' },
        },
        {
            '[[',
            function()
                Snacks.words.jump(-vim.v.count1)
            end,
            desc = 'Prev Reference',
            mode = { 'n', 't' },
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
