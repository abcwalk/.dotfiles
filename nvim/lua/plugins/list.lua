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
    --     'MeanderingProgrammer/py-requirements.nvim',
    --     lazy = false,
    --     dependencies = { 'nvim-treesitter/nvim-treesitter' },
    --     config = function()
    --         local requirements = require('py-requirements')
    --         vim.keymap.set('n', ',ru', requirements.upgrade, { silent = true, desc = 'Requirements: Upgrade' })
    --         vim.keymap.set('n', ',rU', requirements.upgrade_all, { silent = true, desc = 'Requirements: Upgrade All' })
    --         vim.keymap.set(
    --             'n',
    --             ',rK',
    --             requirements.show_description,
    --             { silent = true, desc = 'Requirements: Show package description' }
    --         )
    --         requirements.setup({})
    --     end,
    -- },
    -- {
    --     'romgrk/barbar.nvim',
    --     enabled = true,
    --     dependencies = 'nvim-web-devicons',
    --     lazy = false,
    -- },
    -- {
    --     'echasnovski/mini.tabline',
    --     version = '*',
    --     lazy = false,
    --     config = function()
    --         require('mini.tabline').setup()
    --     end,
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
    --     config = load_config('tools.neotree'),
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
    --     'MeanderingProgrammer/render-markdown.nvim',
    --     -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
    --     -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
    --     dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
    --     lazy = false,
    --     config = function()
    --         require('render-markdown').setup()
    --     end,
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
    -- {
    --     'shadowofseaice/yabs.nvim',
    --     lazy = false,
    --     config = load_config('tools.yabs'),
    -- },
    -- {
    --     'b0o/incline.nvim',
    --     config = function()
    --         require('incline').setup()
    --     end,
    --     -- Optional: Lazy load Incline
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
    -- },
    -- {
    --   'phaazon/hop.nvim',
    --     lazy = false,
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
    --         require('deepwhite').setup({
    --             low_blue_light = false,
    --         })
    --         vim.cmd([[colorscheme deepwhite]])
    --         -- vim.api.nvim_set_hl(0, 'WarningMsg', { bg = 'none', fg = '#f27900' })
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
    --     'folke/todo-comments.nvim',
    --     lazy = false,
    --     dependencies = { 'nvim-lua/plenary.nvim' },
    --     opts = {
    --         -- your configuration comes here
    --         -- or leave it empty to use the default settings
    --         -- refer to the configuration section below
    --     },
    -- },
    {
        'kevinhwang91/nvim-hlslens',
        config = load_config('tools.hlslens'),
        lazy = false,
    },
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
    {
        'akinsho/toggleterm.nvim',
        event = 'ColorScheme',
        version = '*',
        config = load_config('tools.toggle-term'),
    },
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
    -- {
    --     'FabijanZulj/blame.nvim',
    --     lazy = false,
    --     config = function()
    --         require('blame').setup()
    --     end,
    -- },
    -- {
    --     'echasnovski/mini.indentscope',
    --     lazy = false,
    --     config = function()
    --         vim.api.nvim_create_autocmd('FileType', {
    --             pattern = {
    --                 'NeogitPopup',
    --                 'Trouble',
    --                 'alpha',
    --                 'dashboard',
    --                 'floaterm',
    --                 'help',
    --                 'lazy',
    --                 'lazyterm',
    --                 'mason',
    --                 'neo-tree',
    --                 'neogit',
    --                 'notify',
    --                 'toggleterm',
    --             },
    --             callback = function()
    --                 vim.b.miniindentscope_disable = true
    --             end,
    --         })
    --         require('mini.indentscope').setup({
    --             draw = {
    --                 animation = require('mini.indentscope').gen_animation.none(),
    --             },
    --         })
    --     end,
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
    -- 'MagicDuck/grug-far.nvim',
    -- lazy = false,
    -- config = function()
    --   require('grug-far').setup({
    --       keymaps = {
    --             close = { n = 'q' },
    --       }
    --   });
    -- end
    -- },
    -- {
    --     'lukas-reineke/indent-blankline.nvim',
    --     event = { 'BufReadPost', 'BufNewFile' },
    --     main = 'ibl',
    --     config = load_config('ui.indent-blankline'),
    -- },

    -- UI
    -- {
    --     'bluz71/vim-moonfly-colors',
    --     name = 'moonfly',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.g.moonflyNormalFloat = true
    --         vim.g.moonflyWinSeparator = 2
    --         vim.g.moonflyTransparent = false
    --         vim.g.moonflyItalics = false
    --         vim.cmd('colorscheme moonfly')
    --         vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
    --         vim.api.nvim_set_hl(0, 'Pmenu', { link = 'Normal' })
    --         vim.api.nvim_set_hl(0, 'LineNr', { bg = 'none', fg = '#626262' })
    --         vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'none' })
    --         vim.api.nvim_set_hl(0, 'SignColumn', { bg = 'none' })
    --         vim.api.nvim_set_hl(0, 'LazyButton', { link = 'Normal' })
    --         vim.api.nvim_set_hl(0, 'FlashLabel', { link = 'MoonflyBlueMode' })
    --         vim.api.nvim_set_hl(0, 'DiffAdd', { link = 'MoonflyGreen' })
    --         vim.api.nvim_set_hl(0, 'DiffDelete', { link = 'MoonflyRed' })
    --         vim.api.nvim_set_hl(0, 'DiffChange', { link = 'MoonflyOrange' })
    --         vim.api.nvim_set_hl(0, 'OilVcsStatusUntracked', { link = 'MoonflyGrey80' })
    --     end,
    -- },
    -- Lazy
    -- {
    --     'oonamo/ef-themes.nvim',
    --     config = function()
    --         require('ef-themes').setup({
    --             transparent = false,
    --             on_highlights = function(highlights, colors, name)
    --                 return {
    --                     FloatBorder = { bg = 'none' },
    --                     Pmenu = { bg = 'none' },
    --                 }
    --             end,
    --         })
    --         vim.cmd.colorscheme('ef-symbiosis')
    --     end,
    --     lazy = false,
    --     priority = 1000,
    -- },
    -- {
    --     'folke/tokyonight.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         require('tokyonight').setup({
    --             styles = {
    --                 -- Style to be applied to different syntax groups
    --                 -- Value is any valid attr-list value for `:help nvim_set_hl`
    --                 comments = { italic = true },
    --                 keywords = { italic = false },
    --                 functions = {},
    --                 variables = {},
    --                 -- Background styles. Can be "dark", "transparent" or "normal"
    --                 sidebars = 'dark', -- style for sidebars, see below
    --                 floats = 'dark', -- style for floating windows
    --             },
    --         })
    --         vim.cmd('colorscheme tokyonight-night')
    --     end,
    -- },
    -- {
    --     'nickkadutskyi/jb.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     opts = {},
    --     config = function()
    --         -- require("jb").setup({transparent = true})
    --         vim.cmd('colorscheme jb')
    --         -- vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'none' })
    --         vim.api.nvim_set_hl(0, 'Float', { bg = 'none' })
    --         vim.api.nvim_set_hl(0, 'Pmenu', { bg = 'None' })
    --     end,
    -- },
    -- {
    --     'thesimonho/kanagawa-paper.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     opts = {},
    --     config = function()
    --         require('kanagawa-paper').setup({
    --             undercurl = true,
    --             transparent = false,
    --             gutter = false,
    --             dimInactive = true, -- disabled when transparent
    --             terminalColors = true,
    --             commentStyle = { italic = true },
    --             functionStyle = { italic = false },
    --             keywordStyle = { italic = false, bold = false },
    --             statementStyle = { italic = false, bold = false },
    --             typeStyle = { italic = false },
    --             colors = { theme = {}, palette = {} }, -- override default palette and theme colors
    --             overrides = function() -- override highlight groups
    --                 return {
    --                     Pmenu = { bg = 'none' },
    --                     FloatBorder = { fg = 'none' },
    --                 }
    --             end,
    --         })
    --         vim.cmd('colorscheme kanagawa-paper')
    --     end,
    -- },
    -- {
    --     'rjshkhr/shadow.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.opt.termguicolors = true
    --         vim.cmd.colorscheme('shadow')
    --     end,
    -- },
    -- {
    --     'svrana/neosolarized.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         require('neosolarized').setup({
    --             comment_italics = false,
    --             background_set = true,
    --         })
    --         vim.cmd.colorscheme('neosolarized')
    --     end,
    --     dependencies = {
    --         'tjdevries/colorbuddy.nvim',
    --     },
    -- },
    -- {
    --     'maxmx03/solarized.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     opts = {
    --         variant = 'winter', -- "spring" | "summer" | "autumn" | "winter" (default)
    --         -- transparent = { enabled = true, pmenu = false, normalfloat = false },
    --         styles = {
    --             comments = { italic = false, bold = false },
    --             functions = { italic = false },
    --             variables = { italic = false },
    --         },
    --         -- palette = 'selenized',
    --         on_highlights = function(_, _)
    --             local groups = {
    --                 -- Light
    --                 -- Identifier = { fg = 'Black' },
    --                 -- Property = { fg = 'Black' },
    --                 -- Search = { fg = '#489100', bg = '#e9e9c5', bold = true },
    --                 -- Parameter = { fg = 'Black' }, -- test
    --                 -- CmpItemAbbr = { fg = 'Black' },
    --                 -- CmpItemAbbrMatch = { link = 'DiagnosticSignWarn' },
    --                 -- CmpItemAbbrMatchFuzzy = { link = 'DiagnosticSignWarn' },
    --                 -- PmenuSel = { fg = '#909995' },
    --
    --                 -- Dark
    --                 Identifier = { fg = '#93a1a1' },
    --                 Property = { fg = '#93a1a1' },
    --                 Parameter = { fg = '#93a1a1' }, -- test
    --                 Keyword = { fg = '#93a1a1', bold = false },
    --                 StatusLine = { fg = '#909995', bg = '#002731' },
    --                 ['@string.documentation.python'] = { link = 'String' },
    --                 Type = { fg = '#b58900' },
    --                 ['@variable.builtin.python'] = { link = 'Function' },
    --                 ['@keyword.function.python'] = { link = 'Statement' },
    --                 ['@keyword.import.python'] = { fg = '#cb4b16' },
    --                 ['@keyword.type.python'] = { link = 'Statement' },
    --                 ['@keyword.python'] = { link = 'Statement' },
    --                 ['@type.builtin.python'] = { link = 'Type' },
    --                 -- ['@variable.member.python'] = { link = 'Constant' },
    --
    --                 CmpItemAbbr = { fg = '#93a1a1' },
    --                 CmpItemAbbrMatch = { fg = '#b58900' },
    --                 CmpItemAbbrMatchFuzzy = { fg = '#b58900' },
    --
    --                 OilVcsStatusAdded = { link = 'DiagnosticSignOk' },
    --                 OilVcsStatusCopied = { link = 'DiagnosticSignOk' },
    --                 OilVcsStatusDeleted = { link = 'DiagnosticSignError' },
    --                 OilVcsStatusIgnored = { link = 'LineNr' },
    --                 OilVcsStatusModified = { link = 'DiagnosticSignWarn' },
    --                 OilVcsStatusUnModified = { link = 'LineNr' },
    --                 OilVcsStatusRenamed = { link = 'DiagnosticSignWarn' },
    --                 OilVcsStatusUntracked = { link = 'DiagnosticSignHint' },
    --                 OilVcsStatusUpstreamAdded = { link = 'DiagnosticSignOk' },
    --                 OilVcsStatusUpstreamCopied = { link = 'DiagnosticSignOk' },
    --                 OilVcsStatusUpstreamDeleted = { link = 'DiagnosticSignError' },
    --                 OilVcsStatusUpstreamIgnored = { link = 'LineNr' },
    --                 OilVcsStatusUpstreamModified = { link = 'DiagnosticSignWarn' },
    --                 OilVcsStatusUpstreamUnModified = { link = 'LineNr' },
    --                 OilVcsStatusUpstreamRenamed = { link = 'DiagnosticSignWarn' },
    --                 OilVcsStatusUpstreamUntracked = { link = 'DiagnosticSignHint' },
    --             }
    --             return groups
    --         end,
    --     },
    --     config = function(_, opts)
    --         vim.o.termguicolors = true
    --         vim.o.background = 'dark'
    --         require('solarized').setup(opts)
    --         vim.cmd.colorscheme('solarized')
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
    {
        'p00f/alabaster.nvim',
        lazy = false,
        priority = 1000,
        config = function()
            vim.g.alabaster_dim_comments = true
            vim.g.alabaster_floatborder = true
            vim.cmd('colorscheme alabaster')
            vim.api.nvim_set_hl(0, 'OilVcsStatusModified', { bg = 'none', fg = '#e3b341' })
            vim.api.nvim_set_hl(0, 'OilVcsStatusUntracked', { bg = 'none', fg = '#cc8bc9' })
            vim.api.nvim_set_hl(0, 'OilVcsStatusDeleted', { bg = 'none', fg = '#f85149' })
        end,
    },
    -- {
    --     'chrishrb/gx.nvim',
    --     keys = { { 'gx', '<cmd>Browse<cr>', mode = { 'n', 'x' } } },
    --     cmd = { 'Browse' },
    --     init = function()
    --         vim.g.netrw_nogx = 1 -- disable netrw gx
    --     end,
    --     dependencies = { 'nvim-lua/plenary.nvim' }, -- Required for Neovim < 0.10.0
    --     config = true, -- default settings
    -- },
    -- {
    --     'shadowy-pycoder/vscode-gruber.nvim',
    --     dependencies = { 'rktjmp/lush.nvim' },
    --     name = 'vscode-gruber',
    --     branch = 'main',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme vscode-gruber')
    --     end,
    -- },
    -- {
    --     'rockyzhang24/arctic.nvim',
    --     branch = 'v2',
    --     dependencies = { 'rktjmp/lush.nvim' },
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme vscode-gruber')
    --     end,
    -- },
    -- {
    --     'behemothbucket/alabaster.nvim',
    --     lazy = false,
    --     branch = 'custom', -- main | custom
    --     priority = 1000,
    --     config = function()
    --         vim.g.alabaster_dim_comments = true
    --         vim.g.alabaster_floatborder = true
    --         vim.cmd('colorscheme alabaster')
    --     end,
    -- },
    -- {
    --     'cpwrs/americano.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         require('americano').setup({
    --         terminal = true,
    --       })
    --       vim.cmd('colorscheme americano')
    --     end,
    -- },
    -- {
    --     'alvarosevilla95/luatab.nvim',
    --     dependencies = { 'nvim-tree/nvim-web-devicons' },
    --     lazy = false,
    --     config = function()
    --         require('luatab').setup()
    --     end,
    -- },
    -- {
    --     'nyoom-engineering/oxocarbon.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.o.background = 'dark'
    --         vim.cmd('colorscheme oxocarbon')
    --     end,
    -- },
    -- {
    --     'F4LCn/oxocharcoal.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.oxocharcoal'),
    -- },
    -- {
    --     'projekt0n/github-nvim-theme',
    --     lazy = false, -- make sure we load this during startup if it is your main colorscheme
    --     priority = 1000, -- make sure to load this before all the other start plugins
    --     config = function()
    --         require('github-theme').setup({
    --             -- ...
    --         })
    --         vim.o.background = 'dark'
    --         vim.cmd('colorscheme github_dark_tritanopia') -- more muted colors
    --     end,
    -- },
    -- {
    --     'ribru17/bamboo.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         require('bamboo').setup({
    --             style = 'multiplex', -- Choose between 'vulgaris' (regular), 'multiplex' (greener), and 'light'
    --             code_style = {
    --                 comments = { italic = false },
    --                 conditionals = { italic = false },
    --                 keywords = {},
    --                 functions = {},
    --                 namespaces = { italic = false },
    --                 parameters = { italic = false },
    --                 strings = {},
    --                 variables = {},
    --             },
    --         })
    --         require('bamboo').load()
    --     end,
    -- },
    -- {
    --     'samharju/serene.nvim',
    --     lazy = false, -- make sure we load this during startup if it is your main colorscheme
    --     priority = 1000, -- make sure to load this before all the other start plugins
    --     config = function()
    --         vim.cmd.colorscheme('serene')
    --     end,
    -- },
    -- Lazy
    -- {
    --     'phha/zenburn.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.o.background = 'light'
    --         vim.cmd('colorscheme zenburn')
    --     end,
    -- },
    -- {
    --     'andymass/vim-matchup',
    --     lazy = false,
    --     config = function()
    --         vim.g.matchup_matchparen_offscreen = { method = 'popup' }
    --     end,
    -- },
    --     'yorickpeterse/nvim-grey',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.o.background = 'light'
    --         vim.cmd('colorscheme grey')
    --     end,
    -- },
    --     'yorickpeterse/nvim-grey',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.o.background = 'light'
    --         vim.cmd('colorscheme grey')
    --     end,
    -- },
    -- {
    --     'slugbyte/lackluster.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     init = function()
    --         vim.o.background = 'dark'
    --         vim.cmd.colorscheme('lackluster-hack') -- my favorite
    --     end,
    -- },
    -- {
    --     'behemothbucket/mellow.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.mellow'),
    -- },
    -- {
    --     'behemothbucket/nano-theme.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     init = function()
    --         vim.o.background = 'light'
    --         vim.cmd('colorscheme nano-theme')
    --     end,
    -- },
    -- {
    --     'yorickpeterse/nvim-grey',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme grey')
    --     end,
    -- },
    -- {
    --     'xiantang/darcula-dark.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd('colorscheme darcula-dark')
    --     end,
    -- },
    -- { 'EdenEast/nightfox.nvim', lazy = false, config = load_config('ui.nightfox') },
    -- {
    --     'NTBBloodbath/doom-one.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = function()
    --         -- Add color to cursor
    --         vim.g.doom_one_cursor_coloring = true
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
    --         -- vim.g.doom_one_pumblend_transparency = 20
    --
    --         -- Plugins integration
    --         -- vim.g.doom_one_plugin_neorg = true
    --         -- vim.g.doom_one_plugin_barbar = false
    --         vim.g.doom_one_plugin_telescope = true
    --         -- vim.g.doom_one_plugin_neogit = true
    --         -- vim.g.doom_one_plugin_nvim_tree = true
    --         -- vim.g.doom_one_plugin_dashboard = true
    --         -- vim.g.doom_one_plugin_startify = true
    --         vim.g.doom_one_plugin_whichkey = true
    --         -- vim.g.doom_one_plugin_indent_blankline = false
    --         -- vim.g.doom_one_plugin_vim_illuminate = false
    --         vim.g.doom_one_plugin_lspsaga = true
    --         vim.o.background = 'dark'
    --         vim.cmd('colorscheme doom-one')
    --         vim.api.nvim_set_hl(0, 'Pmenu', { bg = 'None' })
    --         -- vim.api.nvim_set_hl(0, 'OilVcsStatusUntracked', { fg = 'NvimDarkCyan' })
    --         vim.api.nvim_set_hl(0, 'FlashLabel', { fg = '#ff6c6b' })
    --     end,
    -- },
    {
        'goolord/alpha-nvim',
        dependencies = { 'echasnovski/mini.icons' },
        lazy = false,
        config = function()
            require('alpha').setup(require('alpha.themes.startify').config)
        end,
    },
    -- {
    --     'RRethy/vim-illuminate',
    --     lazy = false,
    --     config = function()
    --         require('illuminate').configure({
    --             -- min_count_to_highlight = 2,
    --             filetypes_denylist = {
    --                 'dirbuf',
    --                 'dirvish',
    --                 'fugitive',
    --                 'blame',
    --                 'alpha',
    --                 'oil',
    --             },
    --             under_cursor = false,
    --         })
    --         -- change the highlight style
    --         vim.api.nvim_set_hl(0, 'IlluminatedWordText', { link = 'Visual' })
    --         vim.api.nvim_set_hl(0, 'IlluminatedWordRead', { link = 'Visual' })
    --         vim.api.nvim_set_hl(0, 'IlluminatedWordWrite', { link = 'Visual' })
    --
    --         --- auto update the highlight style on colorscheme change
    --         vim.api.nvim_create_autocmd({ 'ColorScheme' }, {
    --             pattern = { '*' },
    --             callback = function(v)
    --                 vim.api.nvim_set_hl(0, 'IlluminatedWordText', { link = 'Visual' })
    --                 vim.api.nvim_set_hl(0, 'IlluminatedWordRead', { link = 'Visual' })
    --                 vim.api.nvim_set_hl(0, 'IlluminatedWordWrite', { link = 'Visual' })
    --             end,
    --         })
    --     end,
    -- },
    -- {
    --     'echasnovski/mini.cursorword',
    --     version = '*',
    --     lazy = false,
    --     config = function()
    --         require('mini.cursorword').setup()
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
    --     'maxmx03/solarized.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     opts = {},
    --     config = function(_, opts)
    --         vim.o.termguicolors = true
    --         vim.o.background = 'light'
    --         require('solarized').setup(opts)
    --         vim.cmd.colorscheme('solarized')
    --     end,
    -- },
    -- {
    --     'craftzdog/solarized-osaka.nvim',
    --     lazy = false,
    --     priority = 1000,
    --     config = load_config('ui.solarized-osaka'),
    --     -- config = function()
    --     --     vim.o.background = 'dark'
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
    {
        'nvim-lualine/lualine.nvim',
        config = load_config('ui.lualine'),
        lazy = false,
    },
    -- https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line.html
    -- https://github.com/nvimdev/modeline.nvim
    -- {
    --     'behemothbucket/modeline.nvim',
    --     config = function()
    --         require('modeline').setup()
    --     end,
    --     lazy = false,
    -- },
    -- {
    --     'bluz71/nvim-linefly',
    --     lazy = false,
    --     config = function()
    --         vim.g.linefly_options = {
    --             tabline = false,
    --             winbar = false,
    --             with_lsp_status = true,
    --             with_search_count = true,
    --             with_spell_status = true,
    --             with_indent_status = true,
    --         }
    --     end,
    -- },
    {
        'stevearc/dressing.nvim',
        config = load_config('ui.dressing'),
        event = { 'BufReadPre', 'BufNewFile' },
    },

    -- Language
    -- {
    --     'ray-x/go.nvim',
    --     dependencies = { -- optional packages
    --         'ray-x/guihua.lua',
    --     },
    --     event = { 'CmdlineEnter' },
    --     config = load_config('lang.go'),
    --     ft = { 'go', 'gomod' },
    --     build = ':lua require("go.install").update_all_sync()', -- if you need to install/update all binaries
    --     lazy = false,
    -- },
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
    -- {
    --     'hrsh7th/nvim-cmp',
    --     dependencies = {
    --         'hrsh7th/cmp-buffer',
    --         'hrsh7th/cmp-path',
    --         'hrsh7th/cmp-nvim-lsp',
    --         'hrsh7th/cmp-nvim-lua',
    --         'saadparwaiz1/cmp_luasnip',
    --         'lukas-reineke/cmp-under-comparator',
    --     },
    --     config = load_config('lang.cmp'),
    --     lazy = false,
    --     event = 'InsertEnter',
    -- },
    { 'L3MON4D3/LuaSnip' },
    {
        'xzbdmw/nvim-cmp',
        dependencies = {
            'neovim/nvim-lspconfig',
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-buffer',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-cmdline',
            'hrsh7th/nvim-cmp',
            'saadparwaiz1/cmp_luasnip',
        },
        lazy = false,
        event = 'InsertEnter',
        config = load_config('lang.cmp'),
    },
    { 'saadparwaiz1/cmp_luasnip' },
    -- Tresitter
    {
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate',
        lazy = false,
        config = load_config('lang.treesitter'),
    },
    {
        'nvim-treesitter/nvim-treesitter-context',
        dependencies = { 'nvim-treesitter/nvim-treesitter' },
        lazy = false,
        config = function()
            require('treesitter-context').setup({
                enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
                max_lines = 2, -- How many lines the window should span. Values <= 0 mean no limit.
                min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
                line_numbers = false,
                multiline_threshold = 20, -- Maximum number of lines to show for a single context
                trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
                mode = 'cursor', -- Line used to calculate context. Choices: 'cursor', 'topline'
                -- Separator between context and content. Should be a single character string, like '-'.
                -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
                separator = ' ',
                zindex = 20, -- The Z-index of the context window
                on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
            })
            vim.api.nvim_set_hl(0, 'TreesitterContext', { link = 'Statusline' })
        end,
    },
    -- LSP
    { 'onsails/lspkind.nvim' },
    -- Noice
    -- {
    --     'folke/noice.nvim',
    --     event = 'VeryLazy',
    --     opts = {
    --         -- add any options here
    --     },
    --     dependencies = {
    --         -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
    --         'MunifTanjim/nui.nvim',
    --         -- OPTIONAL:
    --         --   `nvim-notify` is only needed, if you want to use the notification view.
    --         --   If not available, we use `mini` as the fallback
    --         -- 'rcarriga/nvim-notify',
    --     },
    --     config = function()
    --         require('noice').setup({
    --             lsp = {
    --                 -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
    --                 override = {
    --                     ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
    --                     ['vim.lsp.util.stylize_markdown'] = true,
    --                     ['cmp.entry.get_documentation'] = true, -- requires hrsh7th/nvim-cmp
    --                 },
    --             },
    --             -- you can enable a preset for easier configuration
    --             -- presets = {
    --             --     bottom_search = true, -- use a classic bottom cmdline for search
    --             --     command_palette = true, -- position the cmdline and popupmenu together
    --             --     long_message_to_split = true, -- long messages will be sent to a split
    --             --     inc_rename = false, -- enables an input dialog for inc-rename.nvim
    --             --     lsp_doc_border = false, -- add a border to hover docs and signature help
    --             -- },
    --         })
    --     end,
    -- },
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
    -- {
    --     'yetone/avante.nvim',
    --     event = 'VeryLazy',
    --     lazy = false,
    --     version = false, -- set this if you want to always pull the latest change
    --     opts = {
    --         -- add any opts here
    --     },
    --     -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
    --     build = 'make',
    --     -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
    --     dependencies = {
    --         'stevearc/dressing.nvim',
    --         'nvim-lua/plenary.nvim',
    --         'MunifTanjim/nui.nvim',
    --         --- The below dependencies are optional,
    --         'nvim-tree/nvim-web-devicons', -- or echasnovski/mini.icons
    --         'zbirenbaum/copilot.lua', -- for providers='copilot'
    --         {
    --             -- support for image pasting
    --             'HakonHarnes/img-clip.nvim',
    --             event = 'VeryLazy',
    --             opts = {
    --                 -- recommended settings
    --                 default = {
    --                     embed_image_as_base64 = false,
    --                     prompt_for_file_name = false,
    --                     drag_and_drop = {
    --                         insert_mode = true,
    --                     },
    --                     -- required for Windows users
    --                     use_absolute_path = true,
    --                 },
    --             },
    --         },
    --         {
    --             -- Make sure to set this up properly if you have lazy=true
    --             'MeanderingProgrammer/render-markdown.nvim',
    --             opts = {
    --                 file_types = { 'markdown', 'Avante' },
    --             },
    --             ft = { 'markdown', 'Avante' },
    --         },
    --     },
    -- },
    -- {
    --     'Wansmer/symbol-usage.nvim',
    --     event = 'LspAttach', -- need run before LspAttach if you use nvim 0.9. On 0.10 use 'LspAttach'
    --     config = load_config('lang.symbol-usage'),
    -- },
    -- {
    --     'AckslD/swenv.nvim',
    --     lazy = false,
    --     config = load_config('lang.swenv'),
    -- },

    -- {
    --     'linux-cultist/venv-selector.nvim',
    --     dependencies = {
    --         'neovim/nvim-lspconfig',
    --         'mfussenegger/nvim-dap',
    --         'mfussenegger/nvim-dap-python', --optional
    --         { 'nvim-telescope/telescope.nvim', branch = '0.1.x', dependencies = { 'nvim-lua/plenary.nvim' } },
    --     },
    --     lazy = false,
    --     branch = 'regexp', -- This is the regexp branch, use this for the new version
    --     config = function()
    --         require('venv-selector').setup()
    --     end,
    --     keys = {
    --         { ',v', '<cmd>VenvSelect<cr>' },
    --     },
    -- },
    {
        'echasnovski/mini.misc',
        version = '*',
        config = function()
            require('mini.misc').setup()
            require('mini.misc').setup_auto_root()
        end,
    },
    {
        'alexpasmantier/pymple.nvim',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'MunifTanjim/nui.nvim',
            -- optional (nicer ui)
            'stevearc/dressing.nvim',
            'nvim-tree/nvim-web-devicons',
        },
        build = ':PympleBuild',
        lazy = false,
        config = function()
            require('pymple').setup({
                python = {
                    root_markers = { 'pyproject.toml', 'setup.py', '.git', 'manage.py' },
                    -- the names of virtual environment folders to look out for when
                    -- discovering a project
                    virtual_env_names = { '.venv', 'venv' },
                },
            })
        end,
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

    -- {
    --     'saghen/blink.cmp',
    --     -- optional: provides snippets for the snippet source
    --     dependencies = 'rafamadriz/friendly-snippets',
    --
    --     -- use a release tag to download pre-built binaries
    --     version = '*',
    --     -- AND/OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
    --     -- build = 'cargo build --release',
    --     -- If you use nix, you can build from source using latest nightly rust with:
    --     -- build = 'nix run .#build-plugin',
    --
    --     ---@module 'blink.cmp'
    --     ---@type blink.cmp.Config
    --     opts = {
    --         -- 'default' for mappings similar to built-in completion
    --         -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
    --         -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
    --         -- See the full "keymap" documentation for information on defining your own keymap.
    --         keymap = {
    --             preset = 'none',
    --             ['<CR>'] = { 'accept', 'fallback' },
    --             ['<Tab>'] = { 'select_next', 'fallback' },
    --             ['<S-Tab>'] = { 'select_prev', 'fallback' },
    --             ['<Up>'] = { 'select_prev', 'fallback' },
    --             ['<Down>'] = { 'select_next', 'fallback' },
    --             ['<C-p>'] = { 'select_prev', 'fallback' },
    --             ['<C-n>'] = { 'select_next', 'fallback' },
    --             ['<C-c>'] = { 'hide' },
    --             ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
    --             ['<C-k>'] = { 'show_signature', 'hide_signature', 'fallback' },
    --         },
    --
    --         completion = {
    --             accept = {
    --                 create_undo_point = true,
    --                 auto_brackets = {
    --                     enabled = true,
    --                     default_brackets = { '(', ')', '{', '}', '[', ']' },
    --                     kind_resolution = {
    --                         enabled = true,
    --                     },
    --                     semantic_token_resolution = {
    --                         enabled = true,
    --                     },
    --                 },
    --             },
    --             menu = {
    --                 border = 'rounded',
    --                 auto_show = function(ctx)
    --                     return not vim.tbl_contains({ '/', '?' }, vim.fn.getcmdtype())
    --                 end,
    --             },
    --             list = {
    --                 selection = {
    --                     preselect = function(ctx)
    --                         return ctx.mode ~= 'cmdline'
    --                     end,
    --                     auto_insert = true,
    --                 },
    --             },
    --             documentation = { window = { border = 'single' } },
    --             ghost_text = {
    --                 enabled = true,
    --                 -- Show the ghost text when an item has been selected
    --                 show_with_selection = true,
    --                 -- Show the ghost text when no item has been selected, defaulting to the first item
    --                 show_without_selection = true,
    --             },
    --         },
    --         signature = { window = { border = 'rounded' } },
    --
    --         appearance = {
    --             -- Sets the fallback highlight groups to nvim-cmp's highlight groups
    --             -- Useful for when your theme doesn't support blink.cmp
    --             -- Will be removed in a future release
    --             use_nvim_cmp_as_default = false,
    --             -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
    --             -- Adjusts spacing to ensure icons are aligned
    --             nerd_font_variant = 'mono',
    --         },
    --
    --         -- Displays a preview of the selected item on the current line
    --
    --         -- Default list of enabled providers defined so that you can extend it
    --         -- elsewhere in your config, without redefining it, due to `opts_extend`
    --         sources = {
    --             default = { 'lsp', 'path', 'snippets', 'buffer' },
    --         },
    --     },
    --     opts_extend = { 'sources.default' },
    -- },
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs and related tools to stdpath for Neovim
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',
            'WhoIsSethDaniel/mason-tool-installer.nvim',
            -- 'saghen/blink.cmp',

            -- Useful status updates for LSP.
            -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
            -- {
            --     'j-hui/fidget.nvim',
            --     opts = {
            --         notification = {
            --             window = {
            --                 winblend = 0,
            --             },
            --         },
            --     },
            -- },
            -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
            -- used for completion, annotations and signatures of Neovim apis
            -- { 'folke/neodev.nvim', opts = {} },
        },
        config = load_config('lang.lspconfig'),
        lazy = false,
    },
    -- {
    --     'ray-x/lsp_signature.nvim',
    --     event = 'VeryLazy',
    --     opts = {},
    --     config = function(_, opts)
    --         require('lsp_signature').setup(opts)
    --     end,
    -- },
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
    -- {
    --     'kosayoda/nvim-lightbulb',
    --     lazy = false,
    --     config = function()
    --         require('nvim-lightbulb').setup({
    --             sign = {
    --                 enabled = true,
    --                 -- Text to show in the sign column.
    --                 -- Must be between 1-2 characters.
    --                 text = '',
    --                 -- Highlight group to highlight the sign column text.
    --                 hl = 'Comment',
    --             },
    --             autocmd = { enabled = true },
    --         })
    --     end,
    -- },
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
    --     'kiyoon/python-import.nvim',
    --     lazy = false,
    --     -- build = "pipx install . --force",
    --     build = 'uv tool install . --force --reinstall',
    --     keys = {
    --         {
    --             '<Space>a',
    --             function()
    --                 require('python_import.api').add_import_current_word_and_notify()
    --             end,
    --             mode = { 'i', 'n' },
    --             silent = true,
    --             desc = 'Add python import',
    --             ft = 'python',
    --         },
    --         {
    --             '<Space>0.1.5',
    --             function()
    --                 require('python_import.api').add_import_current_selection_and_notify()
    --             end,
    --             mode = 'x',
    --             silent = true,
    --             desc = 'Add python import',
    --             ft = 'python',
    --         },
    --         -- {
    --         --     '<space>i',
    --         --     function()
    --         --         require('python_import.api').add_import_current_word_and_move_cursor()
    --         --     end,
    --         --     mode = 'n',
    --         --     silent = true,
    --         --     desc = 'Add python import and move cursor',
    --         --     ft = 'python',
    --         -- },
    --         -- {
    --         --     '<space>i',
    --         --     function()
    --         --         require('python_import.api').add_import_current_selection_and_move_cursor()
    --         --     end,
    --         --     mode = 'x',
    --         --     silent = true,
    --         --     desc = 'Add python import and move cursor',
    --         --     ft = 'python',
    --         -- },
    --         {
    --             '<space>tr',
    --             function()
    --                 require('python_import.api').add_rich_traceback()
    --             end,
    --             silent = true,
    --             desc = 'Add rich traceback',
    --             ft = 'python',
    --         },
    --     },
    --     opts = {
    --         -- Example 1:
    --         -- Default behaviour for `tqdm` is `from tqdm.auto import tqdm`.
    --         -- If you want to change it to `import tqdm`, you can set `import = {"tqdm"}` and `import_from = {tqdm = nil}` here.
    --         -- If you want to change it to `from tqdm import tqdm`, you can set `import_from = {tqdm = "tqdm"}` here.
    --
    --         -- Example 2:
    --         -- Default behaviour for `logger` is `import logging`, ``, `logger = logging.getLogger(__name__)`.
    --         -- If you want to change it to `import my_custom_logger`, ``, `logger = my_custom_logger.get_logger()`,
    --         -- you can set `statement_after_imports = {logger = {"import my_custom_logger", "", "logger = my_custom_logger.get_logger()"}}` here.
    --         extend_lookup_table = {
    --             ---@type string[]
    --             import = {
    --                 -- "tqdm",
    --             },
    --
    --             ---@type table<string, string>
    --             import_as = {
    --                 -- These are the default values. Here for demonstration.
    --                 -- np = "numpy",
    --                 -- pd = "pandas",
    --             },
    --
    --             ---@type table<string, string>
    --             import_from = {
    --                 -- tqdm = nil,
    --                 -- tqdm = "tqdm",
    --             },
    --
    --             ---@type table<string, string[]>
    --             statement_after_imports = {
    --                 -- logger = { "import my_custom_logger", "", "logger = my_custom_logger.get_logger()" },
    --             },
    --         },
    --
    --         ---Return nil to indicate no match is found and continue with the default lookup
    --         ---Return a table to stop the lookup and use the returned table as the result
    --         ---Return an empty table to stop the lookup. This is useful when you want to add to wherever you need to.
    --         ---@type fun(winnr: integer, word: string, ts_node: TSNode?): string[]?
    --         custom_function = function(winnr, word, ts_node)
    --             -- if vim.endswith(word, "_DIR") then
    --             --   return { "from my_module import " .. word }
    --             -- end
    --         end,
    --     },
    -- },
    {
        'NeogitOrg/neogit',
        dependencies = {
            'nvim-lua/plenary.nvim', -- required
            'sindrets/diffview.nvim', -- optional - Diff integration
        },
        lazy = false,
        config = function()
            require('neogit').setup({
                disable_context_highlighting = true,
                mappings = {
                    popup = {
                        ['P'] = 'PushPopup',
                        ['p'] = 'PushPopup',
                        ['F'] = 'PullPopup',
                    },
                },
            })
        end,
    },
    {
        'danymat/neogen',
        -- Uncomment next line if you want to follow only stable versions
        version = '*',
        lazy = false,
        config = function()
            require('neogen').setup({
                enabled = true,
                snippet_engine = 'luasnip',
                languages = {
                    python = {
                        template = {
                            annotation_convention = 'numpydoc', -- reST | google_docstrings | numpydoc
                        },
                    },
                },
            })
        end,
    },
    {
        'wurli/visimatch.nvim',
        opts = {},
        lazy = false,
    },
    -- {
    --     'stevanmilic/nvim-lspimport',
    --     lazy = false,
    --     config = function()
    --         vim.keymap.set('n', '<leader>a', require('lspimport').import, { noremap = true })
    --     end,
    -- },
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
    -- minimal installation
    -- {
    --     'Hashino/doing.nvim',
    --     cmd = 'Do',
    --     keys = {
    --         {
    --             '<leader>ta',
    --             function()
    --                 require('doing').add()
    --             end,
    --             { desc = '[D]oing: [A]dd' },
    --         },
    --         {
    --             '<leader>tn',
    --             function()
    --                 require('doing').done()
    --             end,
    --             {},
    --         },
    --         {
    --             '<leader>te',
    --             function()
    --                 require('doing').edit()
    --             end,
    --             {},
    --         },
    --         {
    --             '<leader>tt',
    --             function()
    --                 require('doing').toggle()
    --             end,
    --             {},
    --         },
    --     },
    --     },
    -- },
    {
        'rachartier/tiny-glimmer.nvim',
        event = 'VeryLazy',
        opts = {
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
                    default_animation = 'reverse_fade',
                    -- paste_mapping = 'p',
                    -- Paste_mapping = 'P',
                },
                undo = {
                    enabled = true,
                    default_animation = {
                        name = 'fade',
                        settings = {
                            from_color = 'DiffDelete',
                            min_duration = 1000,
                        },
                    },
                },
                redo = {
                    enabled = false,
                    default_animation = {
                        name = 'fade',
                        settings = {
                            from_color = 'DiffAdd',
                        },
                    },
                },
                -- transparency_color = '#000000',
            },
        },
    },
    {
        'folke/which-key.nvim',
        event = 'VeryLazy',
        opts = {
            preset = 'modern',
            win = {
                title = true,
            },
        },
        keys = {
            {
                '<Space>?',
                function()
                    require('which-key').show({ global = false })
                end,
                desc = 'Buffer Local Keymaps (which-key)',
            },
        },
    },
    {
        'leath-dub/snipe.nvim',
        lazy = false,
        keys = {
            {
                '<Tab><Tab>',
                function()
                    require('snipe').open_buffer_menu()
                end,
                desc = 'Open Snipe buffer menu',
            },
            {
                '<leader><Tab>',
                function()
                    require('snipe').open_buffer_menu()
                end,
                desc = 'Open Snipe buffer menu',
            },
        },
        config = function()
            require('snipe').setup({
                ui = {
                    position = 'cursor',
                },
                -- The default sort used for the buffers
                -- Can be any of "last", (sort buffers by last accessed) "default" (sort buffers by its number)
                sort = 'last',
                -- Limit the width of path buffer names
                -- /my/long/path/is/really/annoying will be is/really/annoying (max of 3)
                -- max_path_width = 3,
            })
        end,
    },
    -- {
    --     'voxelprismatic/rabbit.nvim',
    --     lazy = false,
    --     config = function()
    --         require('rabbit').setup({
    --             window = {
    --                 width = 36,
    --                 height = 16,
    --                 float = 'center',
    --             },
    --             default_keys = {
    --                 close = { '<Esc>', 'q', '<leader>' },
    --                 select = { '<CR>' },
    --                 open = { '<Tab><Tab>' },
    --                 file_add = { 'a' },
    --                 file_del = { '<Del>' },
    --                 group = { 'A' },
    --                 group_up = { '-' },
    --             },
    --         })
    --     end,
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
