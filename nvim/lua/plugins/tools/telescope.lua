local status_ok, telescope = pcall(require, 'telescope')
if not status_ok then
    return
end

telescope.setup({
    -- extensions = {
    --     ['ui-select'] = {
    --         require('telescope.themes').get_ivy({}),
    --     },
    --     specific_opts = {
    --         codeactions = false,
    --     },
    -- },
    defaults = {
        file_ignore_patterns = {
            'node_modules',
            'venv',
            '.venv',
            '__pycache__',
        },
    },
    pickers = {
        find_files = {
            theme = 'ivy',
        },
        live_grep = {
            theme = 'ivy',
        },
        grep_string = {
            theme = 'ivy',
        },
        lsp_references = {
            theme = 'ivy',
        },
        oldfiles = {
            theme = 'ivy',
        },
        diagnostics = {
            theme = 'ivy',
            layout_config = {
                vertical = { width = 0.5 },
            },
        },
        buffers = {
            theme = 'ivy',
        },
    },
})
pcall(require('telescope').load_extension, 'fzf')
pcall(require('telescope').load_extension, 'projects')
-- pcall(require('telescope').load_extension, 'ui-select')

local builtin = require('telescope.builtin')

-- Slightly advanced example of overriding default behavior and theme
vim.keymap.set('n', '<leader>/', function()
    -- You can pass additional configuration to Telescope to change the theme, layout, etc.
    builtin.current_buffer_fuzzy_find(require('telescope.themes').get_ivy({
        previewer = false,
    }))
end)

-- Shortcut for searching your Neovim configuration files
-- vim.keymap.set('n', '<leader>cs', function()
--     builtin.find_files({ cwd = vim.fn.stdpath('config') })
-- end)
