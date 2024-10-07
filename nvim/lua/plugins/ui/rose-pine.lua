local status_ok, rose_pine = pcall(require, 'rose-pine')
if not status_ok then
    return
end

rose_pine.setup({
    variant = 'main', -- auto, main, moon, or dawn
    dark_variant = 'main', -- main, moon, or dawn
    dim_inactive_windows = false,
    extend_background_behind_borders = true,

    enable = {
        terminal = true,
        legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
        migrations = true, -- Handle deprecated options automatically
    },

    styles = {
        bold = true,
        italic = false,
        transparency = true,
    },

    highlight_groups = {
        Normal = { bg = 'none' },
        FloatBorder = { bg = 'none' },
        CursorLine = { bg = 'none' },
        StatusLine = { bg = '#1a1815' },
        -- Normal = { bg = '#1b160a' },
        -- ColorColumn = { bg = '#c1c1c' },
        -- Cursor = { bg = '#c5c9c5', fg = '#181618' },
        -- MatchParen = { bg = 'none', fg = '#338fff', bold = true },
        OilVcsStatusAdded = { bg = 'none', fg = '#31748f' },
        OilVcsStatusCopied = { bg = 'none', fg = '#403d52' },
        OilVcsStatusDeleted = { bg = 'none', fg = '#eb6f92' },
        OilVcsStatusIgnored = { bg = 'none', fg = '#6e6a86' },
        OilVcsStatusModified = { bg = 'none', fg = '#f6c177' },
        OilVcsStatusRenamed = { bg = 'none', fg = '#ebbcba' },
        OilVcsStatusUntracked = { bg = 'none', fg = '#c4a7e7' },
        OilVcsStatusUpstreamAdded = { bg = 'none', fg = '#31748f' },
        OilVcsStatusUpstreamCopied = { bg = 'none', fg = '#403d52' },
        OilVcsStatusUpstreamDeleted = { bg = 'none', fg = '#eb6f92' },
        OilVcsStatusUpstreamIgnored = { bg = 'none', fg = '#6e6a86' },
        OilVcsStatusUpstreamModified = { bg = 'none', fg = '#f6c177' },
        OilVcsStatusUpstreamRenamed = { bg = 'none', fg = '#ebbcba' },
        OilVcsStatusUpstreamUntracked = { bg = 'none', fg = '#c4a7e7' },
        CmpItemAbbrDeprecated = { bg = 'none', strikethrough = true, fg = '#808080' },
        CmpItemAbbrMatch = { bg = 'none', fg = '#569CD6' },
        CmpItemAbbrMatchFuzzy = { link = 'CmpIntemAbbrMatch' },
        CmpItemKindVariable = { bg = 'none', fg = '#9ccfd8' },
        CmpItemKindInterface = { link = 'CmpItemKindVariable' },
        CmpItemKindText = { link = 'CmpItemKindVariable' },
        CmpItemKindFunction = { bg = 'none', fg = '#c4a7e7' },
        CmpItemKindMethod = { link = 'CmpItemKindFunction' },
        CmpItemKindKeyword = { link = 'CmpItemKindFunction' },
        CmpItemKindProperty = { link = 'CmpItemKindKeyword' },
        CmpItemKindUnit = { link = 'CmpItemKindKeyword' },
    },
})

vim.cmd('colorscheme rose-pine')
