local status_ok, toggleterm = pcall(require, 'toggleterm')
if not status_ok then
    return
end

local opts = {
    open_mapping = [[<C-Bslash>]],
    shade_filetypes = {},
    direction = 'horizontal',
    autochdir = true,
    persist_mode = true,
    insert_mappings = false,
    start_in_insert = true,
    highlights = {
        -- highlights which map to a highlight group name and a table of it's values
        -- NOTE: this is only a subset of values, any group placed here will be set for the terminal window split
        Normal = {
            link = 'Normal',
        },
        NormalFloat = {
            link = 'NormalFloat',
        },
        FloatBorder = {
            link = 'FloatBorder',
        },
        EndOfBuffer = {
            link = 'EndOfBuffer',
        },
        SignColumn = {
            link = 'SignColumn',
        },
        StatusLine = {
            link = 'StatusLine',
        },
        StatusLineNC = {
            link = 'StatusLineNC',
        },
        WinBar = {
            link = 'WinBar',
        },
        WinBarNC = {
            link = 'WinBarNC',
        },
    },
    size = function(term)
        if term.direction == 'horizontal' then
            return 15
        elseif term.direction == 'vertical' then
            return math.floor(vim.o.columns * 0.4)
        end
    end,
}

toggleterm.setup(opts)
