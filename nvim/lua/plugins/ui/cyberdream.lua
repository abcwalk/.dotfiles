local status_ok, cyberdream = pcall(require, 'cyberdream')
if not status_ok then
    return
end

local colors = require('cyberdream.colors').default
cyberdream.setup({
    transparent = true,
    italic_comments = false,
    hide_fillchars = true,
    borderless_telescope = false,
    terminal_colors = true,
    theme = {
        highlights = {
            Normal = { fg = '#cecece' },
            Constant = { fg = colors.magenta },
            StatusLine = { fg = '#cecece' },
            ModeMsg = { link = 'Comment' },
            MsgArea = { link = 'Comment' },
        },
    },
})
vim.cmd('colorscheme cyberdream')
