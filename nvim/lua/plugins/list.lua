local function load_config(package)
    return function()
        require('plugins.' .. package)
    end
end

local plugins = {
    -- Tools
    -- {
    --     'tpope/vim-sleuth',
    --     lazy = false,
    -- },
    {
        'asiryk/auto-hlsearch.nvim',
        tag = '1.1.0',
        lazy = false,
        config = function()
            require('auto-hlsearch').setup()
        end,
    },
    -- {
    --     'behemothbucket/cyberdream.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.cyberdream'),
    -- },
    {
        'kdheepak/lazygit.nvim',
        cmd = {
            'LazyGit',
            'LazyGitConfig',
            'LazyGitCurrentFile',
            'LazyGitFilter',
            'LazyGitFilterCurrentFile',
        },
        -- optional for floating window border decoration
        dependencies = {
            'nvim-lua/plenary.nvim',
        },
        -- setting the keybinding for LazyGit with 'keys' is recommended in
        -- order to load the plugin when the command is run for the first time
        keys = {
            { '<leader>lg', '<cmd>LazyGit<cr>', desc = 'LazyGit' },
        },
    },
    -- {
    --     'romgrk/barbar.nvim',
    --     enabled = true,
    --     dependencies = 'nvim-web-devicons',
    --     config = function()
    --         vim.api.nvim_set_hl(0, 'BufferDefaultTabpageFill', { bg = 'none' })
    --     end,
    --     lazy = false,
    -- },
    -- {
    --     'nvim-neo-tree/neo-tree.nvim',
    --     branch = 'v3.x',
    --     lazy = false,
    --     dependencies = {
    --         'nvim-lua/plenary.nvim',
    --         'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    --         'MunifTanjim/nui.nvim',
    --         -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
    --     },
    --     config = function()
    --         require('neo-tree').setup({
    --             close_if_last_window = true,
    --         })
    --     end,
    -- },
    -- {
    --     'nvim-tree/nvim-tree.lua',
    --     enabled = true,
    --     dependencies = {
    --         'nvim-tree/nvim-web-devicons',
    --     },
    --     config = load_config('tools.nvim-tree'),
    -- },
    -- {
    --     'otavioschwanck/arrow.nvim',
    --     opts = {
    --         show_icons = true,
    --         leader_key = ';',
    --     },
    --     config = load_config('tools.arrow'),
    --     lazy = false,
    -- },
    -- {
    --     'tzachar/local-highlight.nvim',
    --     config = function()
    --         require('local-highlight').setup({
    --             -- file_types = { 'python', 'cpp' }, -- If this is given only attach to this
    --             -- OR attach to every filetype except:
    --             -- disable_file_types = { 'tex' },
    --             -- hlgroup = 'Search',
    --             -- -- cw_hlgroup = nil,
    --             -- Whether to display highlights in INSERT mode or not
    --             insert_mode = false,
    --             min_match_len = 2,
    --             max_match_len = math.huge,
    --         })
    --     end,
    --     lazy = false,
    -- },
    {
        'shadowofseaice/yabs.nvim',
        lazy = false,
        config = load_config('tools.yabs'),
    },
    -- {
    --     'b0o/incline.nvim',
    --     config = function()
    --         require('incline').setup()
    --     end,
    -- Optional: Lazy load Incline
    --     event = 'VeryLazy',
    -- },
    {
        'NvChad/nvim-colorizer.lua',
        enabled = true,
        lazy = false,
        config = load_config('tools.colorizer'),
    },
    -- {
    --     'mhinz/vim-startify',
    --     lazy = false,
    -- },
    -- {
    --   'phaazon/hop.nvim',
    --   branch = 'v2',
    --   config = function()
    --     require('hop').setup({})
    --     --Hop
    --     local hop = require('hop')
    --     local map = vim.keymap.set
    --     local directions = require('hop.hint').HintDirection
    --     map('', 'f', function()
    --       hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false })
    --     end, { remap = true })
    --     map('', 'F', function()
    --       hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = false })
    --     end, { remap = true })
    --     map('', 't', function()
    --       hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
    --     end, { remap = true })
    --     map('', 'T', function()
    --       hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
    --     end, { remap = true })
    --     map('n', 'ff', ':HopChar2<CR>')
    --   end,
    --   lazy = false,
    -- },
    {
        'numToStr/Comment.nvim',
        config = load_config('tools.comment'),
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
        'kylechui/nvim-surround',
        config = load_config('tools.surround'),
        lazy = false,
    },
    -- {
    --     'chrisgrieser/nvim-spider',
    --     config = load_config('tools.spider'),
    -- },
    -- {
    --     'Verf/deepwhite.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd([[colorscheme deepwhite]])
    --         vim.api.nvim_set_hl(0, "WarningMsg", { bg = "none", fg = "#f27900" })
    --     end,
    -- },
    {
        'abecodes/tabout.nvim',
        config = load_config('tools.tabout'),
        lazy = false,
        event = 'InsertCharPre',
        priority = 1000,
    },
    -- {
    --     '2kabhishek/tdo.nvim',
    --     dependencies = 'nvim-telescope/telescope.nvim',
    --     cmd = { 'Tdo', 'TdoEntry', 'TdoNote', 'TdoTodos', 'TdoToggle', 'TdoFind', 'TdoFiles' },
    --     keys = { '[t', ']t' },
    --     lazy = false,
    -- },
    -- {
    --     'kevinhwang91/nvim-hlslens',
    --     config = load_config('tools.hlslens'),
    --     lazy = false,
    -- },
    {
        'folke/flash.nvim',
        config = load_config('tools.flash'),
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
        'ahmedkhalf/project.nvim',
        lazy = false,
        config = load_config('tools.project'),
    },
    -- {
    --     'akinsho/toggleterm.nvim',
    --     event = 'ColorScheme',
    --     version = '*',
    --     config = load_config('tools.toggle-term'),
    -- },
    -- {
    --     'iamcco/markdown-preview.nvim',
    --
    --     build = function()
    --         vim.fn['mkdp#util#install']()
    --     end,
    --     ft = 'markdown',
    --     cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview' },
    -- },
    -- {
    --   'nvim-neo-tree/neo-tree.nvim',
    --   branch = 'v3.x',
    --   lazy = false,
    --
    --   cmd = 'Neotree',
    --
    --   dependencies = {
    --     'nvim-lua/plenary.nvim',
    --     'nvim-tree/nvim-web-devicons',       -- not strictly required, but recommended
    --     'MunifTanjim/nui.nvim',
    --     -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
    --   },
    --   config = load_config('tools.neo-tree'),
    -- },
    {
        'stevearc/oil.nvim',
        config = load_config('tools.oil'),
        lazy = false,
    },
    {
        'SirZenith/oil-vcs-status',
        config = load_config('tools.oil-vcs-status'),
        lazy = false,
    },
    -- {
    --     'vidocqh/auto-indent.nvim',
    --     lazy = false,
    -- },
    {
        'windwp/nvim-autopairs',
        event = 'InsertEnter',
        lazy = false,
        config = load_config('tools.autopairs'),
    },
    {
        'sindrets/diffview.nvim',
        opts = {
            view = {
                merge_tool = {
                    layout = 'diff3_mixed',
                },
            },
        },
        lazy = false,
    },
    {
        'windwp/nvim-spectre',
        config = load_config('tools.spectre'),
        cmd = 'Spectre',
    },
    -- {
    --     'lukas-reineke/indent-blankline.nvim',
    --     event = { 'BufReadPost', 'BufNewFile' },
    --     main = 'ibl',
    --     config = load_config('ui.indent-blankline'),
    -- },

    -- UI
    -- {
    --     'rebelot/kanagawa.nvim',
    --     config = load_config('ui.kanagawa'),
    --     lazy = false,
    --     priority = 1000,
    -- },
    -- {
    --     'bluz71/vim-moonfly-colors',
    --     name = 'moonfly',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme moonfly')
    --     end,
    -- },
    -- {
    --     'Mofiqul/vscode.nvim',
    --     lazy = false,
    --     config = load_config('ui.vscode-theme'),
    -- },
    -- {
    --     'rockerBOO/boo-colorscheme-nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         require('boo-colorscheme').use({
    --             italic = false,
    --             theme = 'boo',
    --         })
    --     end,
    -- },
    -- {
    --     'metalelf0/jellybeans-nvim',
    --     dependencies = { 'rktjmp/lush.nvim' },
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme jellybeans-nvim')
    --     end,
    -- },
    -- {
    --     'behemothbucket/alabaster.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme alabaster')
    --     end,
    -- },
    { 'EdenEast/nightfox.nvim', lazy = false, config = load_config('ui.nightfox') },
    -- {
    --     'NTBBloodbath/doom-one.nvim',
    --     lazy = false,
    --     config = function()
    --         -- Add color to cursor
    --         vim.g.doom_one_cursor_coloring = false
    --         -- Set :terminal colors
    --         vim.g.doom_one_terminal_colors = true
    --         -- Enable italic comments
    --         vim.g.doom_one_italic_comments = false
    --         -- Enable TS support
    --         vim.g.doom_one_enable_treesitter = true
    --         -- Color whole diagnostic text or only underline
    --         vim.g.doom_one_diagnostics_text_color = false
    --         -- Enable transparent background
    --         vim.g.doom_one_transparent_background = false
    --
    --         -- Pumblend transparency
    --         vim.g.doom_one_pumblend_enable = false
    --         vim.g.doom_one_pumblend_transparency = 20
    --
    --         -- Plugins integration
    --         -- vim.g.doom_one_plugin_neorg = true
    --         -- vim.g.doom_one_plugin_barbar = false
    --         vim.g.doom_one_plugin_telescope = false
    --         -- vim.g.doom_one_plugin_neogit = true
    --         vim.g.doom_one_plugin_nvim_tree = true
    --         -- vim.g.doom_one_plugin_dashboard = true
    --         -- vim.g.doom_one_plugin_startify = true
    --         -- vim.g.doom_one_plugin_whichkey = true
    --         -- vim.g.doom_one_plugin_indent_blankline = true
    --         -- vim.g.doom_one_plugin_vim_illuminate = true
    --         vim.g.doom_one_plugin_lspsaga = false
    --         vim.cmd('colorscheme doom-one')
    --     end,
    -- },
    -- {
    --     'mikesmithgh/gruvsquirrel.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme gruvsquirrel')
    --     end,
    -- },
    -- {
    --     'marko-cerovac/material.nvim',
    --
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme material-deep-ocean')
    --     end,
    -- },
    -- {
    --     'rose-pine/neovim',
    --     name = 'rose-pine',
    --     lazy = false,
    --     config = load_config('ui.rose-pine'),
    -- },
    -- {
    --     'behemothbucket/base16-black-metal-scheme',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme base16-black-metal')
    --     end,
    -- },
    -- {
    --     'behemothbucket/gruber-darker-theme.nvim',
    --     config = function()
    --         require('gruber-darker').setup()
    --         vim.cmd('colorscheme gruber-darker')
    --     end,
    --     priority = 1000,
    --     lazy = false,
    -- },
    -- {
    --     'behemothbucket/dirty-ice-theme.nvim',
    --     config = function()
    --         require('dirty-ice').setup()
    --         vim.cmd('colorscheme dirty-ice')
    --     end,
    --     lazy = false,
    -- },
    -- {
    --     'jesseleite/nvim-noirbuddy',
    --     dependencies = {
    --         { 'tjdevries/colorbuddy.nvim', branch = 'dev' },
    --     },
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.noirbuddy'),
    -- },
    -- {
    --     "water-sucks/darkrose.nvim",
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.darkrose')
    -- },
    -- {
    --     'miikanissi/modus-themes.nvim',
    --     priority = 1000,
    --     lazy = false,
    --     -- config = load_config('ui.modus')
    -- },
    -- {
    --     'craftzdog/solarized-osaka.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.solarized-osaka'),
    --     -- config = function()
    --     --     vim.cmd('colorscheme solarized-osaka')
    --     -- end,
    -- },
    -- {
    --     'Mofiqul/adwaita.nvim',
    --     lazy = false,
    --     priority = 1000,
    --
    --     -- configure and set on startup
    --     config = function()
    --         vim.g.adwaita_darker = true             -- for darker version
    --         vim.g.adwaita_disable_cursorline = true -- to disable cursorline
    --         vim.g.adwaita_transparent = true        -- makes the background transparent
    --         vim.cmd('colorscheme adwaita')
    --     end,
    -- },
    -- {
    --     'phha/zenburn.nvim',
    --     lazy = false,
    --     config = function()
    --         require('zenburn').setup()
    --         vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'none' })
    --         vim.cmd('colorscheme zenburn')
    --     end,
    -- },
    -- {
    --     'nvim-lualine/lualine.nvim',
    --     config = load_config('ui.lualine'),
    --     event = { 'BufReadPre', 'BufNewFile' },
    -- },
    {
        'stevearc/dressing.nvim',
        config = load_config('ui.dressing'),
        event = { 'BufReadPre', 'BufNewFile' },
    },

    -- Language
    {
        'ray-x/go.nvim',
        dependencies = { -- optional packages
            'ray-x/guihua.lua',
        },
        event = { 'CmdlineEnter' },
        config = load_config('lang.go'),
        ft = { 'go', 'gomod' },
        build = ':lua require("go.install").update_all_sync()', -- if you need to install/update all binaries
        lazy = false,
    },
    {
        -- Linting
        'mfussenegger/nvim-lint',
        event = { 'BufReadPre', 'BufNewFile' },
        config = load_config('lang.lint'),
    },
    -- {
    --     'maxmx03/dracula.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.dracula'),
    -- },
    -- {
    --     'yanskun/gomaps.nvim',
    --     ft = 'go',
    --     config = function()
    --         require('gomaps').setup()
    --     end,
    -- },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'hrsh7th/cmp-buffer',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-nvim-lua',
            'saadparwaiz1/cmp_luasnip',
            'lukas-reineke/cmp-under-comparator',
        },
        config = load_config('lang.cmp'),
        lazy = false,
        event = 'InsertEnter',
    },
    {
        'L3MON4D3/LuaSnip',
        version = 'v2.*',
        dependencies = { 'rafamadriz/friendly-snippets' },
        build = 'make install_jsregexp',
        event = 'InsertEnter',
    },
    -- Tresitter
    {
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate',
        lazy = false,
        config = load_config('lang.treesitter'),
    },
    -- LSP
    -- {
    --     'folke/neodev.nvim',
    --     ft = { 'lua', 'vim' },
    --     config = load_config('lang.neodev'),
    -- },
    -- {
    --     'theprimeagen/refactoring.nvim',
    --     config = function()
    --         require('refactoring').setup({})
    --     end,
    -- }
    {
        'Wansmer/symbol-usage.nvim',
        event = 'LspAttach', -- need run before LspAttach if you use nvim 0.9. On 0.10 use 'LspAttach'
        config = load_config('lang.symbol-usage'),
    },
    {
        'AckslD/swenv.nvim',
        lazy = false,
        config = load_config('lang.swenv'),
    },
    -- {
    --     'VonHeikemen/lsp-zero.nvim',
    --     branch = 'v3.x',
    --     dependencies = {
    --         'neovim/nvim-lspconfig',
    --         'williamboman/mason-lspconfig.nvim',
    --     },
    --     config = load_config('lang.lsp-zero'),
    --     event = { 'BufReadPre', 'BufNewFile' },
    -- },
    {
        'nvimdev/lspsaga.nvim',
        config = load_config('lang.lspsaga'),
        event = 'LspAttach',
    },
    -- {
    --     'zbirenbaum/neodim',
    --     event = 'LspAttach',
    --     config = function()
    --         require('neodim').setup({
    --             refresh_delay = 75,
    --             alpha = 0.75,
    --             blend_color = '#696969',
    --             hide = {
    --                 underline = false,
    --                 virtual_text = true,
    --                 signs = true,
    --             },
    --             regex = {
    --                 '[uU]nused',
    --                 '[nN]ever [rR]ead',
    --                 '[nN]ot [rR]ead',
    --             },
    --             priority = 128,
    --             disable = {},
    --         })
    --     end,
    -- },
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs and related tools to stdpath for Neovim
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',
            'WhoIsSethDaniel/mason-tool-installer.nvim',

            -- Useful status updates for LSP.
            -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
            { 'j-hui/fidget.nvim', opts = {} },
            -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
            -- used for completion, annotations and signatures of Neovim apis
            -- { 'folke/neodev.nvim', opts = {} },
        },
        config = load_config('lang.lspconfig'),
        lazy = false,
    },
    {
        'ray-x/lsp_signature.nvim',
        event = 'VeryLazy',
        opts = {},
        config = function(_, opts)
            require('lsp_signature').setup(opts)
        end,
    },
    -- {
    --     'nvimtools/none-ls.nvim',
    --     config = load_config('lang.none-ls'),
    --     dependencies = {
    --         'jay-babu/mason-null-ls.nvim',
    --     },
    --     event = { 'BufReadPre', 'BufNewFile' },
    -- },
    -- {
    --     'folke/trouble.nvim',
    --     config = function()
    --         require('trouble').setup({
    --             icons = true,
    --         })
    --     end,
    --     lazy = false,
    -- },
    {
        'kosayoda/nvim-lightbulb',
        lazy = false,
        config = function()
            require('nvim-lightbulb').setup({
                sign = {
                    enabled = true,
                    -- Text to show in the sign column.
                    -- Must be between 1-2 characters.
                    text = '',
                    -- Highlight group to highlight the sign column text.
                    hl = 'Comment',
                },
                autocmd = { enabled = true },
            })
        end,
    },
    -- {
    --     'aznhe21/actions-preview.nvim',
    --     config = function()
    --         require('actions-preview').setup({
    --             -- options related to telescope.nvim
    --             telescope = vim.tbl_extend(
    --                 'force',
    --                 -- telescope theme: https://github.com/nvim-telescope/telescope.nvim#themes
    --                 require('telescope.themes').get_ivy(),
    --                 -- a table for customizing content
    --                 {
    --                     -- a function to make a table containing the values to be displayed.
    --                     -- fun(action: Action): { title: string, client_name: string|nil }
    --                     make_value = nil,
    --
    --                     -- a function to make a function to be used in `display` of a entry.
    --                     -- see also `:h telescope.make_entry` and `:h telescope.pickers.entry_display`.
    --                     -- fun(values: { index: integer, action: Action, title: string, client_name: string }[]): function
    --                     make_make_display = nil,
    --                 }
    --             ),
    --         })
    --     end,
    --     lazy = false,
    -- },
    -- { 'nvim-telescope/telescope-ui-select.nvim' },
    {
        'stevearc/conform.nvim',
        config = load_config('lang.conform'),
        lazy = false,
    },
    -- {
    --     'ThePrimeagen/refactoring.nvim',
    --     config = load_config('lang.refactoring'),
    --     lazy = false,
    -- },
    -- {
    --     'j-hui/fidget.nvim',
    --     config = function()
    --         require('fidget').setup()
    --     end,
    --     lazy = false,
    -- },
    -- {
    --     'linrongbin16/lsp-progress.nvim',
    --     config = function()
    --         require('lsp-progress').setup()
    --     end,
    --     lazy = false,
    -- }
    -- {
    --   'zbirenbaum/copilot.lua',
    --   dependencies = {
    --     'zbirenbaum/copilot-cmp',
    --   },
    --   config = load_config('lang.copilot'),
    --   event = 'InsertEnter',
    -- },

    -- Telescope
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.6',
        dependencies = {
            'nvim-lua/plenary.nvim',
            {
                'nvim-telescope/telescope-fzf-native.nvim',
                build = 'make',
            },
            { 'nvim-tree/nvim-web-devicons' },
        },
        config = load_config('tools.telescope'),
        cmd = 'Telescope',
    },

    -- Git
    {
        'lewis6991/gitsigns.nvim',
        config = load_config('tools.gitsigns'),
        cmd = 'Gitsigns',
        lazy = false,
    },
    {
        'tpope/vim-fugitive',
        cmd = 'Git',
    },
}

return {
    plugins = plugins,
}
