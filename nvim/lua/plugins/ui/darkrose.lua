local status_ok, darkrose = pcall(require, 'darkrose')
if not status_ok then
    return
end

darkrose.setup({
    -- Override existing or add new highlight groups
    overrides = function(c)
        return {
        Normal = { bg = 'none' },
        FloatBorder = { bg = 'none' },
        StatusLine = { bg = 'none' },
        CursorLine = { bg = 'none' },
        }
    end,
     styles = {
        bold = true, -- Enable bold highlights for some highlight groups
        italic = false, -- Enable italic highlights for some highlight groups
        underline = false, -- Enable underline highlights for some highlight groups
    }
})
vim.cmd.colorscheme("darkrose")
