return {
    {
        'folke/noice.nvim',
        event = 'VeryLazy',
        enabled = true,
        dependencies = {
            -- 'rcarriga/nvim-notify',
            'MunifTanjim/nui.nvim',
        },
        config = function()
            require('noice').setup({
                lsp = {
                    override = {
                        ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
                        ['vim.lsp.util.stylize_markdown'] = true,
                        ['cmp.entry.get_documentation'] = false,
                    },
                    progress = {
                        enabled = false,
                    },
                    hover = {
                        enabled = false,
                    },
                    signature = {
                        enabled = false,
                        auto_open = {
                            enabled = false,
                        },
                    },
                },
                presets = {
                    bottom_search = true, -- use a classic bottom cmdline for search
                    command_palette = false, -- position the cmdline and popupmenu together
                    long_message_to_split = true, -- long messages will be sent to a split
                    inc_rename = true, -- enables an input dialog for inc-rename.nvim
                    lsp_doc_border = true, -- add a border to hover docs and signature help
                },
                views = {
                    notify = {
                        scrollbar = false,
                    },
                    split = {
                        enter = true,
                        scrollbar = false,
                    },
                    vsplit = {
                        scrollbar = false,
                    },
                    popup = {
                        scrollbar = false,
                    },
                    mini = {
                        scrollbar = false,
                    },
                    cmdline = {
                        scrollbar = false,
                    },
                    cmdline_popup = {
                        scrollbar = false,
                    },
                    cmdline_output = {
                        scrollbar = false,
                    },
                    messages = {
                        scrollbar = false,
                    },
                    confirm = {
                        scrollbar = false,
                    },
                    hover = {
                        scrollbar = false,
                    },
                    popupmenu = {
                        scrollbar = false,
                        win_options = {
                            winhighlight = {
                                Normal = 'NormalFloat', -- change to NormalFloat to make it look like other floats
                                -- FloatBorder = 'NoicePopupmenuBorder', -- border highlight
                                -- CursorLine = 'NoicePopupmenuSelected', -- used for highlighting the selected item
                                -- PmenuMatch = 'NoicePopupmenuMatch', -- used to highlight the part of the item that matches the input
                            },
                        },
                    },
                },
            })
        end,
    },
}
