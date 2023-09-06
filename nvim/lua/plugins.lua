local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  "nvim-tree/nvim-web-devicons",
  "mfussenegger/nvim-jdtls",
  "nvim-lua/plenary.nvim",
  "stevearc/dressing.nvim",
  "sindrets/diffview.nvim",
  "tpope/vim-surround",
  'behemothbucket/gruber-darker-theme.nvim',
  {
    'stevearc/overseer.nvim',
    config = function()
      require('overseer').setup({
        templates = { "java_build" },
        task_list = {
          direction = "bottom",
          max_height = 15,
          default_detail = 1,
          bindings = { ["q"] = function() vim.cmd("OverseerClose") end },
        },
      })
    end
  },
  --  {
  --   'behemothbucket/dirty-ice-theme.nvim',
  --   config = function()
  --     require('dirty-ice').setup()
  --   end
  -- }
  --  { 'toppair/reach.nvim',
  --   config = function()
  --     require('reach').setup({
  --       notifications = true
  --     })
  --     locallazyions = {
  --       handle = 'bufnr',
  --       modified_icon = '•',
  --       previous = {
  --
  --         depth = 1,
  --       },
  --       actions = {
  --         split = 's',
  --         vertsplit = 'v',
  --         tabsplit = 't',
  --         delete = '<Space>',
  --         priority = '=',
  --       },
  --     }
  --     -- require('reach').buffers(options)
  --     vim.keymap.set('n', '<Tab><Tab>', function() require('reach').buffers(options) end, {})
  --   end,
  -- }
  --  {
  --   'shadowofseaice/yabs.nvim',
  --   config = function()
  --     require 'yabs'.setup {
  --       position = { 'C' }, -- {'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N', 'C'}
  --       settings = {
  --         { 'name' },
  --         { 'icon', 'bufnr', 'bufname', 'lnum', 'line' },
  --         { 'path', 'name',  'bufid' },
  --       },
  --       keymap = {
  --         close    = "D",    -- Close buffer. Default D
  --         jump     = "<CR>", -- Jump to buffer. Default <cr>
  --         h_split  = "s",    -- Horizontally split buffer. Default s
  --         v_split  = "v",    -- Vertically split buffer. Default v
  --         pinning  = "p",    -- Open buffer preview. Default p
  --         cycset   = ">",    -- Cycle through settings, Default ]
  --         rcycset  = "<",    -- Reverse cycle through settings, Default [
  --         cycpos   = "}",    -- Cycle through settings, Default >
  --         rcycpos  = "{",    -- Reverse cycle through panel placement, Default <
  --         cycname  = "]",    -- Cycle through file name type, Default }
  --         rcycname = "[",    -- Reverse cycle through file name type, Default {
  --         cychdr   = "T",    -- Cycle through group headerlazyions, Default H
  --         sortpath = "P",    -- Sort by file path. Default P
  --         sortext  = "e",    -- Sort by file extension (type), Default t
  --         sortbuf  = "x",    -- Sort clear = sort by buffer #, default c
  --         sortbase = "b",    -- Sort by file base name #, default f
  --         sortfull = "f",    -- Sort by full file name #, default F
  --         sortsetup = "i",    -- Sort by file name initial #, default i
  --       },
  --       rnu = false,
  --       border = "rounded",
  --       -- symbols = {
  --       -- at most two of these icons can be shown for a given buffer
  --       -- current   = "C", -- default 
  --       -- split     = "S", -- default 
  --       -- alternate = "A", -- default 
  --       -- unloaded  = "H", -- default
  --       -- locked    = "L", -- default 
  --       -- ro        = "R", -- default 
  --       -- edited    = "E", -- default 
  --       -- terminal  = "T", -- default 
  --       -- more      = ">", -- default "", when the panel size is too small for file name
  --       -- grphead   = "-", -- default " ",
  --       -- grptop    = "+", -- default "╭",
  --       -- grpmid    = "|", -- default "│",
  --       -- grpbot    = "+", -- default "╰",
  --       -- pinned    = "P", -- default "",
  --       -- filedef   = "D", -- Filetype icon if not present in nvim-web-devicons. Default 
  --       -- }
  --     }
  --   end,
  -- }
  --  { "tpope/vim-repeat" }
  --  { "rockerBOO/boo-colorscheme-nvim" }
  --  { "romgrk/barbar.nvim" }
  --  { "voldikss/vim-floaterm" }
  "dstein64/vim-startuptime",
  --  { "kkga/vim-envy" }
  "nvim-treesitter/playground",
  "nvim-telescope/telescope.nvim",
  --  {
  --   "m-demare/hlargs.nvim",
  --   config = function()
  --     require("hlargs").setup {
  --       color = '#7a7a7a',
  --     }
  --   end,
  -- }
  {
    "kosayoda/nvim-lightbulb",
    config = function()
      require("nvim-lightbulb").setup({
        autocmd = { enabled = true },
        link_highlights = false,
        sign = {
          enabled = true,
          -- Text to show in the sign column.
          -- Must be between 1-2 characters.
          text = "",
          -- Highlight group to highlight the sign column text.
          hl = "DiagnosticSignHint",
        },
      })
    end
  },
  --  {
  --   "rmagatti/goto-preview",
  --   config = function()
  --     require("goto-preview").setup()
  --   end,
  -- }
  --  {
  --   "rmagatti/auto-session",
  --   config = function()
  --     require("auto-session").setup {
  --       log_level = "error",
  --       auto_session_suppress_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
  --     }
  --   end,
  -- }
  --  {
  --   "rmagatti/alternate-toggler",
  --   config = function()
  --     require("alternate-toggler").setup {
  --       alternates = {
  --         ["=="] = "!=",
  --       },
  --     }
  --   end,
  --   event = { "BufReadPost" },
  -- }
  --  { "cranberry-clockworks/coal.nvim" }
  --  { "itchyny/vim-highlighturl" }
  --  {
  --   "folke/zen-mode.nvim",
  --   config = function()
  --     require("zen-mode").setup {
  --       window = {
  --         width = 120,
  --       },
  --     }
  --   end,
  -- }
  --  {
  --   "nvim-neo-tree/neo-tree.nvim",
  --   branch = "v2.x",
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
  --     "MunifTanjim/nui.nvim",
  --   },
  --   config = function()
  --     vim.fn.sign_define("DiagnosticSignInfo", { text = " ", texthl = "DiagnosticSignInfo" })
  --
  --     require("neo-tree").setup {
  --       default_component_configs = {
  --         icon = {
  --           folder_empty = "",
  --         },
  --       },
  --
  --       filesystem = {
  --         filtered_items = {
  --           hide_dotfiles = false,
  --           hide_gitignored = false,
  --           hide_hidden = false, -- Windows
  --         },
  --       },
  --     }
  --   end,
  -- }
  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs")
    end,
  },
  --  { "akinsho/toggleterm.nvim",version = '*', config = function()
  --   require("toggleterm").setup {
  --     direction = 'float',
  --     float_opts = {
  --       width = 80,
  --       height = 20,
  --     }
  --   }
  -- end }

  --   "sudormrfbin/cheatsheet.nvim",
  --   dependencies = {
  --     { "nvim-telescope/telescope.nvim" },
  --     { "nvim-lua/popup.nvim" },
  --     { "nvim-lua/plenary.nvim" },
  --   },
  -- }
  "romainl/vim-cool",
  --  { "atelierbram/Base4Tone-nvim" }
  --  {
  --   "LeonHeidelbach/trailblazer.nvim",
  --   config = function()
  --     require("trailblazer").setup()
  --   end,
  -- }
  {
    "brenoprata10/nvim-highlight-colors",
    config = function()
      require "config.colors"
    end,
  },
  --  {
  --   "goolord/alpha-nvim",
  --   dependencies = { "nvim-tree/nvim-web-devicons" },
  --   config = function()
  --     require("alpha").setup(require("alpha.themes.startify").config)
  --   end,
  -- }
  {
    "phaazon/hop.nvim",
    branch = "v2",
    config = function()
      require("hop").setup()
    end,
  },
  --  {
  --   "andymass/vim-matchup",
  --   setup = function()
  --     vim.g.matchup_matchparen_offscreen = { method = "popup" }
  --   end,
  -- }
  --  {
  --   "terrortylor/nvim-comment",
  --   config = function()
  --     require("nvim_comment").setup {
  --       comment_empty = false,
  --     }
  --   end,
  -- }
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
    },
    config = function()
      require("config.lsp.init")
    end,
  },
  { "ms-jpq/coq_nvim",      branch = "coq",      build = "python3 -m coq deps" },
  { "ms-jpq/coq.artifacts", branch = "artifacts" },
  {
    "SergioRibera/vim-screenshot",
    build = "npm install --prefix Renderize",
  },
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = {
          "typescript",
          "javascript",
          "css",
          "html",
          "sql",
          "markdown",
          "json5",
          "jsdoc",
          "java",
          "lua",
          "vimdoc",
          "vim",
          "query",
          "c",
          "comment",
          "regex",
        },
        auto_install = true,
        highlight = {
          enable = true,
        },
      }
    end,
  },
  "nvim-treesitter/nvim-treesitter-context",
  {
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      require "config.null_ls"
    end,
  },
  {
    'stevearc/oil.nvim',
    config = function() require('config.oil') end
  },
  --  {
  --   'echasnovski/mini.files',
  --   branch = 'stable',
  --   config = function()
  --     require('mini.files').setup({
  --       mappings = {
  --         close       = 'q',
  --         go_in       = 'I',
  --         go_in_plus  = '<CR>',
  --         go_out      = 'O',
  --         go_out_plus = '<BS>',
  --         reset       = 'c',
  --         reveal_cwd  = '@',
  --         show_help   = '?',
  --         synchronize = '=',
  --         trim_left   = '<',
  --         trim_right  = '>',
  --       },
  --     })
  --   end
  -- }
  --  {
  --   "microsoft/vscode-js-debug",
  --  lazy = true,
  --  build = "npm install --legacy-peer-deps && npx gulp vsDebugServerBundle && mv dist out",
  -- }
  --  { 'mfussenegger/nvim-dap' }
  --
  --  {
  --   "rcarriga/nvim-dap-ui"
  --   dependencies = { "mfussenegger/nvim-dap" },
  --   config = function()
  --     require("dapui").setup()
  --   end
  -- }
  --  {
  --   "mxsdev/nvim-dap-vscode-js",
  --   dependencies = { "mfussenegger/nvim-dap" },
  --   config = function()
  --     require('config.dap.setup')
  --   end,
  -- }
  --  {
  --   "tjdevries/express_line.nvim",
  --   config = function()
  --     local extensions = require "el.extensions"
  --     local subscribe = require "el.subscribe"
  --     local generator = function(_window, buffer)
  --       local segments = {}
  --       table.insert(segments, extensions.mode)
  --       table.insert(segments, "%=")
  --       table.insert(
  --         segments,
  --         subscribe.buf_autocmd("el_git_branch", "BufEnter", function(window, buffer)
  --           local branch = extensions.git_branch(window, buffer)
  --           if branch then
  --             return " " .. branch
  --           end
  --         end)
  --       )
  --       table.insert(segments, " [%{&ff}]")
  --       return segments
  --     end
  --     require("el").setup { generator = generator }
  --   end,
  -- }
  --   "nvim-lualine/lualine.nvim",
  --   config = function()
  --     require "config.lualine"
  --   end,
  -- }
  --  { "kdheepak/lazygit.nvim" }
  --  {
  --   "hkupty/iron.nvim",
  --   config = function()
  --     require "config.iron"
  --   end,
  -- }
  --  { "yorickpeterse/nvim-window" }
  --  {
  --   "echasnovski/mini.indentscope",
  --   branch = "stable",
  --   config = function()
  --     local MiniIndent = require "mini.indentscope"
  --     MiniIndent.setup {
  --       draw = {
  --         delay = 0,
  --         animation = MiniIndent.gen_animation.none(),
  --       },
  --     }
  --   end,
  -- }
  --  {
  --   "lewis6991/hover.nvim",
  --   config = function()
  --     require("hover").setup {
  --       setup = function()
  --         -- Require providers
  --         require "hover.providers.lsp"
  --         -- require('hover.providers.gh')
  --         -- require('hover.providers.gh_r')
  --         -- require('hover.providers.jira')
  --         -- require('hover.providers.man')
  --         -- require('hover.providers.dictionary')
  --       end,
  --       preview_opts = {
  --         border = nil,
  --       },
  --       -- Whether the contents of a currently open hover window should be moved
  --       -- to a :h preview-window when pressing the hover keymap.
  --       preview_window = false,
  --       title = true,
  --
  --     -- setup keymaps
  --     vim.keymap.set("n", "K", require("hover").hover, { desc = "hover.nvim" })
  --     vim.keymap.set("n", "gK", require("hover").hover_select, { desc = "hover.nvim (select)" })
  --   end,
  -- }
  --  { "ThePrimeagen/harpoon" }
  "ibhagwan/fzf-lua",
  {
    "gennaro-tedesco/nvim-possession",
    dependencies = {
      "ibhagwan/fzf-lua",
    },
    config = function()
      local possession = require "nvim-possession"
      possession.setup()
      vim.keymap.set("n", "<Bslash><Bslash>", function()
        possession.list()
      end)
      vim.keymap.set("n", "<Space>sn", function()
        possession.new()
      end)
      vim.keymap.set("n", "<Space>su", function()
        possession.update()
      end)
    end,
  },
  {
    "echasnovski/mini.tabline",
    branch = "stable",
    config = function()
      require("mini.tabline").setup()
    end,
  },
  {
    "RRethy/vim-illuminate",
    config = function()
      require('illuminate').configure {
        min_count_to_highlight = 2,
      }
    end
  },
  --  {
  --   "echasnovski/mini.cursorword",
  --   branch = "stable",
  --   config = function()
  --     require("mini.cursorword").setup()
  --   end,
  -- }
  --  {
  --   "lukas-reineke/indent-blankline.nvim",
  --   config = function()
  --     require("indent_blankline").setup {
  --       show_current_context = true,
  --       show_current_context_start = true, -- underline first line
  --       _treesitter = true,
  --     }
  --   end,
  -- }
  --  {
  --   "Wansmer/treesj",
  --   dependencies = { "nvim-treesitter" },
  --   config = function()
  --     require("treesj").setup {
  --       _default_keymaps = false,
  --     }
  --   end,
  -- }
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require "config.gitsigns"
    end,
  },
  --  {
  --   "danymat/neogen",
  --   config = function()
  --     require("neogen").setup {
  --       enabled = true,
  --     }
  --   end,
  -- },
})
