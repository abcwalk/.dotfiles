return {
    {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup({
                padding = true,
                sticky = true,
                ignore = nil,

                toggler = { line = 'gcc', block = 'gbc' },
                opleader = { line = 'gc', block = 'gb' },
                extra = { above = 'gcO', below = 'gco', eol = 'gcA' },
                mappings = { basic = true, extra = true, extended = false },

                pre_hook = nil,
                post_hook = nil,
            })
        end,
        keys = {
            {
                'gcc',
                mode = { 'n' },
                function()
                    require('Comment').toggle()
                end,
                desc = 'Comment',
            },
            {
                'gc',
                mode = { 'v' },
                function()
                    require('Comment').toggle()
                end,
                desc = 'Comment',
            },
        },
    },
    {
        'abecodes/tabout.nvim',
        config = function()
            require('tabout').setup({
                tabkey = '<Tab>',
                backwards_tabkey = '<S-Tab>',
                act_as_tab = true,
                act_as_shift_tab = false,
                default_tab = '<C-t>',
                default_shift_tab = '<C-d>',
                enable_backwards = true,
                completion = false,
                tabouts = {
                    { open = "'", close = "'" },
                    { open = '"', close = '"' },
                    { open = '`', close = '`' },
                    { open = '(', close = ')' },
                    { open = '[', close = ']' },
                    { open = '{', close = '}' },
                },
                ignore_beginning = true,
                exclude = { 'markdown' },
            })
        end,
        event = 'InsertCharPre',
    },
    {
        'folke/flash.nvim',
        config = function()
            require('flash').setup({
                search = {
                    multi_window = false,
                    exclude = {
                        'notify',
                        'cmp_menu',
                        'noice',
                        'flash_prompt',
                        function(win)
                            -- exclude non-focusable windows
                            return not vim.api.nvim_win_get_config(win).focusable
                        end,
                    },
                },
                modes = {
                    search = { enabled = true },
                    char = {
                        enabled = false,
                    },
                },
            })
        end,
        enabled = true,
        event = 'VeryLazy',
        keys = {
            {
                's',
                mode = { 'n', 'x', 'o' },
                function()
                    require('flash').jump()
                end,
                desc = 'Flash',
            },
            {
                'S',
                mode = { 'n', 'x', 'o' },
                function()
                    require('flash').treesitter()
                end,
                desc = 'Flash Treesitter',
            },
        },
    },
    {
        'echasnovski/mini.nvim',
        config = function()
            -- Better Around/Inside textobjects
            --
            -- Examples:
            --  - va)  - [V]isually select [A]round [)]paren
            --  - yinq - [Y]ank [I]nside [N]ext [']quote
            --  - ci'  - [C]hange [I]nside [']quote
            -- require('mini.ai').setup({ n_lines = 500 })

            -- Add/delete/replace surroundings (brackets, quotes, etc.)
            -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
            -- - sd'   - [S]urround [D]elete [']quotes
            -- - sr)'  - [S]urround [R]eplace [)] [']
            require('mini.surround').setup()
            require('mini.pairs').setup()
            require('mini.icons').setup()
        end,
    },
    {
        'j-hui/fidget.nvim',
        config = function()
            require('fidget').setup({})
        end,
    },
    {
        'wurli/visimatch.nvim',
        config = function()
            require('visimatch').setup({})
        end,
    },
    {
        'rachartier/tiny-glimmer.nvim',
        event = 'VeryLazy',
        priority = 10,
        opts = {
            enabled = true,
            refresh_interval_ms = 8,
            overwrite = {
                -- yank = {
                -- enabled = true,
                -- default_animation = {
                --     name = 'fade',
                --     settings = {
                --         -- from_color = 'DiffChange',
                --         min_duration = 1000,
                --     },
                -- },
                -- },
                search = {
                    enabled = false,
                    next_mapping = 'nzzzv',
                    prev_mapping = 'Nzzzv',
                    default_animation = {
                        name = 'pulse',
                        settings = {
                            min_duration = 1000,
                        },
                    },
                },
                paste = {
                    enabled = false,
                    default_animation = {
                        name = 'reverse_fade',
                        settings = {
                            min_duration = 1000,
                        },
                    },
                    -- paste_mapping = 'p',
                    -- Paste_mapping = 'P',
                },
                undo = {
                    enabled = true,
                    default_animation = {
                        name = 'fade',
                        settings = {
                            from_color = 'DiffAdd',
                            min_duration = 1000,
                        },
                    },
                },
                redo = {
                    enabled = false,
                    default_animation = {
                        name = 'fade',
                        settings = {
                            from_color = 'DiffDelete',
                            min_duration = 1000,
                        },
                    },
                },
                -- transparency_color = '#1d2021',
            },
        },
    },
}
