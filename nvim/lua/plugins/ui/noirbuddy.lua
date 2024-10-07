local status_ok, noirbuddy = pcall(require, 'noirbuddy')
if not status_ok then
    return
end

noirbuddy.setup({
    preset = 'minimal',
    styles = {
        italic = false,
        bold = true,
        underline = false,
        undercurl = false,
    },
})

-- Require colorbuddy...
local Color, colors, Group, groups, styles = require('colorbuddy').setup({})

-- Override specific highlight groups...
Group.new('Normal', nil, nil)
Group.new('FloatBorder', nil, nil)
Group.new('StatusLine', nil, nil)
Group.new('SignColumn', nil, nil)
Group.new('CursorLine', nil, nil)
Group.new('CursorLineNR', colors.noir_7, nil)
Group.new('LineNR', nil, nil)
