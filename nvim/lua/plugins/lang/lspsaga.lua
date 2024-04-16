local status_ok, saga = pcall(require, 'lspsaga')
if not status_ok then
    return
end

saga.setup({
    preview = {
        lines_above = 0,
        lines_below = 10,
    },
    scroll_preview = {
        scroll_down = '<C-f>',
        scroll_up = '<C-b>',
    },
    finder = {
        default = 'tyd+ref+imp+def',
        keys = {
            toggle_or_open = '<CR>',
            vsplit = '<C-v>',
            split = '<C-x>',
            quit = 'q',
            -- close = 'q',
        },
    },
    lightbulb = {
        enable = false,
    },
    symbol_in_winbar = {
        enable = false,
        -- separator = '  ',
        hide_keyword = false,
        show_file = true,
        folder_level = 2,
    },
    implement = {
        enable = true,
        sign = true,
        virtual_text = false,
    },
    request_timeout = 2500,
    definition = {
        keys = {
            edit = '<CR>',
            vsplit = '<C-v>',
            split = '<C-x>',
            quit = '<C-c>',
        },
    },
    diagnostic = {
        on_insert = false,
        on_insert_follow = false,
        max_width = 0.4,
        max_show_width = 0.7,
        show_code_action = true,
        jump_num_shortcut = true,
        keys = {
            quit = { 'q', '<ESC>' },
            exec_action = 'o',
        },
    },
    rename = {
        in_select = false,
        keys = {
            quit = '<C-c>',
        },
    },
    code_action = {
        num_shortcut = true,
        show_server_name = true,
        keys = {
            quit = 'q',
            exec = '<CR>',
        },
    },
    ui = {
        theme = 'round',
        border = 'rounded',
        code_action = '',
        diagnostic = '',
        devicon = true,
        title = true,
    },
})
