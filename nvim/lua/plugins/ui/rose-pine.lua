local status_ok, rose_pine = pcall(require, 'rose-pine')
if not status_ok then
    return
end

rose_pine.setup({
    variant = 'auto', -- auto, main, moon, or dawn
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

    groups = {
        border = 'muted',
        link = 'iris',
        panel = 'surface',

        error = 'love',
        hint = 'iris',
        info = 'foam',
        note = 'pine',
        todo = 'rose',
        warn = 'gold',

        git_add = 'foam',
        git_change = 'rose',
        git_delete = 'love',
        git_dirty = 'rose',
        git_ignore = 'muted',
        git_merge = 'iris',
        git_rename = 'pine',
        git_stage = 'iris',
        git_text = 'rose',
        git_untracked = 'subtle',

        h1 = 'iris',
        h2 = 'foam',
        h3 = 'rose',
        h4 = 'gold',
        h5 = 'pine',
        h6 = 'foam',
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
    },
})

vim.cmd('colorscheme rose-pine')

-- TODO
-- FIXME
-- BUG
-- WARN
-- XXX
-- TEST
-- HACK
-- PERF

vim.api.nvim_set_hl(0, 'OilVcsStatusAdded', { bg = 'none', fg = '#31748f' })
vim.api.nvim_set_hl(0, 'OilVcsStatusCopied', { bg = 'none', fg = '#403d52' })
vim.api.nvim_set_hl(0, 'OilVcsStatusDeleted', { bg = 'none', fg = '#eb6f92' })
vim.api.nvim_set_hl(0, 'OilVcsStatusIgnored', { bg = 'none', fg = '#6e6a86' })
vim.api.nvim_set_hl(0, 'OilVcsStatusModified', { bg = 'none', fg = '#f6c177' })
vim.api.nvim_set_hl(0, 'OilVcsStatusRenamed', { bg = 'none', fg = '#ebbcba' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUntracked', { bg = 'none', fg = '#c4a7e7' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamAdded', { bg = 'none', fg = '#31748f' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamCopied', { bg = 'none', fg = '#403d52' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamDeleted', { bg = 'none', fg = '#eb6f92' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamIgnored', { bg = 'none', fg = '#6e6a86' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamModified', { bg = 'none', fg = '#f6c177' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamRenamed', { bg = 'none', fg = '#ebbcba' })
vim.api.nvim_set_hl(0, 'OilVcsStatusUpstreamUntracked', { bg = 'none', fg = '#c4a7e7' })
vim.api.nvim_set_hl(0, 'CmpItemAbbrDeprecated', { bg = 'none', strikethrough = true, fg = '#808080' })
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatch', { bg = 'none', fg = '#569CD6' })
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatchFuzzy', { link = 'CmpIntemAbbrMatch' })
vim.api.nvim_set_hl(0, 'CmpItemKindVariable', { bg = 'none', fg = '#9ccfd8' })
vim.api.nvim_set_hl(0, 'CmpItemKindInterface', { link = 'CmpItemKindVariable' })
vim.api.nvim_set_hl(0, 'CmpItemKindText', { link = 'CmpItemKindVariable' })
vim.api.nvim_set_hl(0, 'CmpItemKindFunction', { bg = 'none', fg = '#c4a7e7' })
vim.api.nvim_set_hl(0, 'CmpItemKindMethod', { link = 'CmpItemKindFunction' })
vim.api.nvim_set_hl(0, 'CmpItemKindKeyword', { link = 'CmpItemKindFunction' })
vim.api.nvim_set_hl(0, 'CmpItemKindProperty', { link = 'CmpItemKindKeyword' })
vim.api.nvim_set_hl(0, 'CmpItemKindUnit', { link = 'CmpItemKindKeyword' })
