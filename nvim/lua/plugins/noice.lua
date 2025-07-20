return {
    'folke/noice.nvim',
    event = 'VeryLazy',
    enabled = true,
    dependencies = {
        'MunifTanjim/nui.nvim',
        'rcarriga/nvim-notify',
    },
    config = function()
        require('noice').setup({
            lsp = {
                override = {
                    ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
                    ['vim.lsp.util.stylize_markdown'] = true,
                    ['cmp.entry.get_documentation'] = true,
                },
                progress = {
                    enabled = false,
                },
                hover = {
                    enabled = false,
                },
            },
            presets = {
                bottom_search = true, -- use a classic bottom cmdline for search
                command_palette = false, -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                inc_rename = false, -- enables an input dialog for inc-rename.nvim
                lsp_doc_border = true, -- add a border to hover docs and signature help
            },
            -- notify = {
            --     view = 'mini',
            -- },
            views = {
                popup = {
                    scrollbar = false,
                },
                cmdline_popup = {
                    align = 'message-bottom',
                    position = { col = '50%', row = '80%' },
                },
                -- mini = {
                --     align = 'message-left',
                --     position = { col = '90%', row = '10%' },
                --     border = {
                --         style = 'rounded',
                --     },
                --     win_options = {
                --         winblend = 0,
                --     },
                -- },
            },
            -- routes = {
            --     {
            --         filter = {
            --             event = 'notify',
            --             find = 'No information available',
            --         },
            --         opts = { skip = true },
            --     },
            -- },
        })
    end,
}
