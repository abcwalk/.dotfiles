local set_color = vim.api.nvim_set_hl

local group_styles = {

    ['Normal'] = { fg = '#cccccc', bg = '#000000' },
    ['NormalFloat'] = { link = 'Normal' },
    ['Comment'] = { fg = '#555555', italic = false },
    ['String'] = { fg = '#bbbbbb', italic = false },
    ['Function'] = { fg = '#aaaaaa', italic = false },
    ['Identifier'] = { fg = '#999999', italic = false },
    ['Special'] = { fg = '#777777', italic = false },
    ['Queston'] = { fg = '#666666', italic = false },
    ['Visual'] = { fg = '#999999', bg = 'NvimDarkGrey4' },

    ['Search'] = { fg = '#000000', bg = '#777777' },
    ['CurSearch'] = { fg = '#000000', bg = '#aaaaaa' },

    ['DiagnosticInfo'] = { fg = '#999999' },
    ['DiagnosticHint'] = { fg = '#999999' },

    ['Directory'] = { fg = '#777777', italic = false },

    ['Pmenu'] = { link = 'Normal' },
    ['PmenuSel'] = { link = 'Search' },

    ['FlashLabel'] = { fg = '#000000', bg = '#cccccc' },
    ['FlashMatch'] = { link = 'Function' },
    ['FlashCurrent'] = { link = 'Function' },

    ['NvimTreeFolderIcon'] = { fg = '#777777', italic = false },

    ['DiffAdd'] = { fg = '#dddddd', bg = 'none', italic = false },
    ['DiffDelete'] = { fg = '#888888', bg = 'none', italic = false },
    ['DiffChange'] = { fg = '#888888', bg = 'none', italic = false },
    ['DiffText'] = { fg = '#000000', bg = '#bbbbbb', italic = false },
    ['Changed'] = { fg = '#eeeeee', bg = 'none', italic = false },
    ['OilVcsUntracked'] = { fg = '#eeeeee', bg = 'none', italic = false },
    ['OilVcsStatusUntracked'] = { fg = '#eeeeee', bg = 'none', italic = false },

    -- INFO:
    -- TODO: Consider to chang the following:
    -- use a non canonical base of the skew symmetric space instead
    -- FIXME: The Einstein summation does not work for all possible tensor dimension!
    -- BUG:
    -- FIXIT:
    -- ISSUE:
    -- WARNING:
    -- WARN:
    -- XXX:
    -- OPTIM:
    -- PERFORMANCE:
    -- PERF:
    -- OPTIMIZE:
    -- TESTING:
    -- TEST:
    -- PASSED:
    -- FAILED:

    ['GitSignsAddInline'] = { fg = '#eeeeee', bg = '#444444', italic = false },
    ['GitSignsDeleteInline'] = { fg = '#aaaaaa', bg = '#333333', italic = false },
}

for group, style in pairs(group_styles) do
    set_color(0, group, style)
end
