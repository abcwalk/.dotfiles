return {
    {
        'lewis6991/gitsigns.nvim',
        lazy = false,
        config = function()
            require('gitsigns').setup({
                signs = {
                    add = { text = '▎' },
                    change = { text = '▎' },
                    delete = { text = '' },
                    topdelete = { text = '' },
                    changedelete = { text = '▎' },
                    untracked = { text = '▎' },
                },
                signs_staged = {
                    add = { text = '▎' },
                    change = { text = '▎' },
                    delete = { text = '' },
                    topdelete = { text = '' },
                    changedelete = { text = '▎' },
                },
                signcolumn = true,
                numhl = false,
                linehl = false,
                word_diff = false,
                watch_gitdir = {
                    interval = 1000,
                    follow_files = true,
                },
                attach_to_untracked = true,
                current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
                current_line_blame_opts = {
                    virt_text = true,
                    virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
                    delay = 1000,
                    ignore_whitespace = false,
                },
                current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
                sign_priority = 6,
                status_formatter = nil,
                update_debounce = 200,
                max_file_length = 40000,
                preview_config = {
                    border = 'rounded',
                    style = 'minimal',
                    relative = 'cursor',
                    row = 0,
                    col = 1,
                },

                on_attach = function(bufnr)
                    vim.keymap.set('n', ']]', require('gitsigns').next_hunk, { buffer = bufnr, desc = 'Next git hunk' })
                    vim.keymap.set(
                        'n',
                        '[[',
                        require('gitsigns').prev_hunk,
                        { buffer = bufnr, desc = 'Previous git hunk' }
                    )
                end,
            })
        end,
        keys = {
            {
                '<leader>gb',
                '<cmd>BlameToggle<CR>',
                desc = 'Blame',
            },
            {
                '<leader>gp',
                function()
                    require('gitsigns').preview_hunk()
                end,
                desc = 'Preview Hunk',
            },
            {
                '<leader>gr',
                function()
                    require('gitsigns').reset_hunk()
                end,
                desc = 'Reset Hunk',
            },
            {
                '<leader>gR',
                function()
                    require('gitsigns').reset_buffer()
                end,
                desc = 'Reset Buffer',
            },
            {
                '<leader>gs',
                function()
                    require('gitsigns').stage_hunk()
                end,
                desc = 'Stage Hunk',
            },
            {
                '<leader>gu',
                function()
                    require('gitsigns').undo_stage_hunk()
                end,
                desc = 'Undo Stage Hunk',
            },
        },
    },
    { 'akinsho/git-conflict.nvim', version = '*', config = true },
}
