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
            -- require('mini.pairs').setup()
            require('mini.icons').setup()
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
                yank = {
                    enabled = true,
                    default_animation = {
                        name = 'fade',
                        settings = {
                            -- from_color = 'DiffChange',
                            min_duration = 1000,
                        },
                    },
                },
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
    {
        'NvChad/nvim-colorizer.lua',
        enabled = true,
        config = function()
            require('colorizer').setup({
                filetypes = { 'lua', 'conf', 'toml' },
                user_default_options = {
                    RGB = true, -- #RGB hex codes
                    RRGGBB = true, -- #RRGGBB hex codes
                    names = false, -- "Name" codes like Blue or blue
                    RRGGBBAA = true, -- #RRGGBBAA hex codes
                    AARRGGBB = true, -- 0xAARRGGBB hex codes
                    rgb_fn = true, -- CSS rgb() and rgba() functions
                    hsl_fn = true, -- CSS hsl() and hsla() functions
                    css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
                    css_fn = true, -- Enable all CSS *functions*: rgb_fn, hsl_fn
                    -- Available modes for `mode`: foreground, background,  virtualtext
                    mode = 'virtualtext', -- Set the display mode.
                    -- Available methods are false / true / "normal" / "lsp" / "both"
                    -- True is same as normal
                    tailwind = false, -- Enable tailwind colors

                    -- parsers can contain values used in |user_default_options|
                    -- sass = {
                    -- enable = true,
                    -- parsers = { 'css' },
                    -- }, -- Enable sass colors
                    virtualtext = '■',
                    -- update color values even if buffer is not focused
                    -- example use: cmp_menu, cmp_docs
                    always_update = false,
                },

                -- all the sub-options of filetypes apply to buftypes
                buftypes = {},
            })
        end,
    },
    {
        'nvchad/showkeys',
        cmd = 'ShowkeysToggle',
        opts = {
            timeout = 1,
            maxkeys = 6,
            -- bottom-left, bottom-right, bottom-center, top-left, top-right, top-center
            position = 'bottom-center',
        },
        keys = {
            {
                '<leader>ut',
                function()
                    vim.cmd('ShowkeysToggle')
                end,
                desc = 'Show key presses',
            },
        },
    },
    {
        'dmtrKovalenko/caps-word.nvim',
        lazy = true,
        opts = {
            enter_callback = function()
                vim.notify('On', vim.log.levels.INFO, { title = 'Caps Word' })
            end,
            exit_callback = function()
                vim.notify('Off', vim.log.levels.INFO, { title = 'Caps Word' })
            end,
        },
        keys = {
            {
                mode = { 'i' },
                '<C-s>',
                "<cmd>lua require('caps-word').toggle()<CR>",
            },
        },
    },
    {
        'FabijanZulj/blame.nvim',
        lazy = false,
        config = function()
            require('blame').setup({})
        end,
    },
    {
        'chrisgrieser/nvim-spider',
        lazy = true,
        keys = {
            { 'w', "<cmd>lua require('spider').motion('w')<CR>", mode = { 'n', 'o', 'x' } },
            { 'e', "<cmd>lua require('spider').motion('e')<CR>", mode = { 'n', 'o', 'x' } },
            { 'b', "<cmd>lua require('spider').motion('b')<CR>", mode = { 'n', 'o', 'x' } },
        },
    },
}
