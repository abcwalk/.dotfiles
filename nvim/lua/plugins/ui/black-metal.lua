vim.cmd('colorscheme base16-black-metal')

local diffadd = '#6abf40'
local diffdelete = '#d2322d'
local diffchange = '#FF9900'
local gray = "#C4C6CD"

vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'none' })
vim.api.nvim_set_hl(0, 'StatusLine', { bg = '#1c1c1c' })
vim.api.nvim_set_hl(0, 'CursorLine', { bg = 'none' })
vim.api.nvim_set_hl(0, 'CursorLineNR', { link = 'String' })
vim.api.nvim_set_hl(0, 'LineNR', { bg = 'none' })
vim.api.nvim_set_hl(0, 'LineNR', { bg = 'none' })
vim.api.nvim_set_hl(0, 'SignColumn', { bg = 'none' })
vim.api.nvim_set_hl(0, 'SignifySignAdd', { bg = 'none', fg = diffadd })
vim.api.nvim_set_hl(0, 'SignifySignChange', { bg = 'none', fg = diffchange })
vim.api.nvim_set_hl(0, 'SignifySignDelete', { bg = 'none', fg = diffdelete })
vim.api.nvim_set_hl(0, 'GitGutterAdd', { bg = 'none', fg = diffadd })
vim.api.nvim_set_hl(0, 'GitGutterChange', { bg = 'none', fg = diffchange })
vim.api.nvim_set_hl(0, 'GitGutterChangeDelete', { bg = 'none', fg = diffchange })
vim.api.nvim_set_hl(0, 'GitGutterDelete', { bg = 'none', fg = diffdelete })
vim.api.nvim_set_hl(0, 'PMenu', { bg = 'none' })
vim.api.nvim_set_hl(0, '@comment.todo.comment', { bg = '#0b1e33', fg = '#4BA8FF' })
vim.api.nvim_set_hl(0, '@comment.note.comment', { bg = '#1b0e23', fg = '#FF57FF' })
vim.api.nvim_set_hl(0, '@comment.warning.comment', { bg = '#392a13', fg = '#FF9900' })
vim.api.nvim_set_hl(0, '@comment.error.comment', { bg = '#391a13', fg = diffchange })

vim.api.nvim_set_hl(0, '@variable', { fg = gray })
vim.api.nvim_set_hl(0, 'FlashLabel', { link = 'String' })
vim.api.nvim_set_hl(0, 'FlashMatch', { fg = gray })

-- TODO
-- NOTE
-- WARN
-- BUG
-- XXX
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

-- gray
vim.api.nvim_set_hl(0, 'CmpItemAbbrDeprecated', { bg = 'NONE', strikethrough = true, fg = '#808080' })
-- blue
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatch', { bg = 'NONE', fg = '#569CD6' })
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatchFuzzy', { link = 'CmpIntemAbbrMatch' })
-- light blue
vim.api.nvim_set_hl(0, 'CmpItemKindVariable', { bg = 'NONE', fg = '#9CDCFE' })
vim.api.nvim_set_hl(0, 'CmpItemKindInterface', { link = 'CmpItemKindVariable' })
vim.api.nvim_set_hl(0, 'CmpItemKindText', { link = 'CmpItemKindVariable' })
-- pink
vim.api.nvim_set_hl(0, 'CmpItemKindFunction', { bg = 'NONE', fg = '#C586C0' })
vim.api.nvim_set_hl(0, 'CmpItemKindMethod', { link = 'CmpItemKindFunction' })
-- front
vim.api.nvim_set_hl(0, 'CmpItemKindKeyword', { link = 'CmpItemKindFunction' })
vim.api.nvim_set_hl(0, 'CmpItemKindProperty', { link = 'CmpItemKindKeyword' })
vim.api.nvim_set_hl(0, 'CmpItemKindUnit', { link = 'CmpItemKindKeyword' })

-- TODO Oil
