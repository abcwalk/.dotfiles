local status_ok, nightfox = pcall(require, 'nightfox')
if not status_ok then
    return
end

nightfox.setup({
    options = {
        -- Compiled file's destination location
        compile_path = vim.fn.stdpath('cache') .. '/nightfox',
        compile_file_suffix = '_compiled', -- Compiled file suffix
        transparent = true, -- Disable setting background
        terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
        dim_inactive = false, -- Non focused panes set to alternative background
        module_default = true, -- Default enable value for modules
        colorblind = {
            enable = false, -- Enable colorblind support
            simulate_only = false, -- Only show simulated colorblind colors and not diff shifted
            severity = {
                protan = 0, -- Severity [0,1] for protan (red)
                deutan = 0, -- Severity [0,1] for deutan (green)
                tritan = 0, -- Severity [0,1] for tritan (blue)
            },
        },
        -- styles = { -- Style to be applied to different syntax groups
        --     comments = 'NONE', -- Value is any valid attr-list value `:help attr-list`
        --     conditionals = 'NONE',
        --     constants = 'NONE',
        --     functions = 'NONE',
        --     keywords = 'NONE',
        --     numbers = 'NONE',
        --     operators = 'NONE',
        --     strings = 'NONE',
        --     types = 'NONE',
        --     variables = 'NONE',
        -- },
        inverse = { -- Inverse highlight for different types
            match_paren = true,
            visual = false,
            search = false,
        },
        -- modules = { -- List of various plugins and additional options
        --     -- ...
        -- },
    },
    -- palettes = {},
    -- specs = {},
    groups = {
        carbonfox = {
            CmpItemAbbrDeprecated = { bg = 'none', fg = '#808080' },
            CmpItemAbbrMatch = { bg = 'none' },
            CmpItemAbbrMatchFuzzy = { link = 'CmpIntemAbbrMatch' },
            CmpItemKindVariable = { bg = 'none' },
            CmpItemKindInterface = { link = 'CmpItemKindVariable' },
            CmpItemKindText = { link = 'CmpItemKindVariable' },
            CmpItemKindFunction = { bg = 'none' },
            CmpItemKindMethod = { link = 'CmpItemKindFunction' },
            CmpItemKindKeyword = { link = 'CmpItemKindFunction' },
            CmpItemKindProperty = { link = 'CmpItemKindKeyword' },
            CmpItemKindUnit = { link = 'CmpItemKindKeyword' },
            NormalFloat = { bg = 'none' },
            FloatBorder = { bg = 'none', fg = 'none' },
            Pmenu = { bg = 'none' },
            WildMenu = { bg = 'none' },
            DiagnosticVirtualTextWarn = { bg = 'none' },
            DiagnosticVirtualTextOk = { bg = 'none' },
            DiagnosticVirtualTextInfo = { bg = 'none' },
            OilVcsStatusAdded = { bg = 'none', fg = 'palette.green' },
            OilVcsStatusCopied = { bg = 'none', fg = 'palette.orange' },
            OilVcsStatusDeleted = { bg = 'none', fg = 'palette.red' },
            OilVcsStatusIgnored = { bg = 'none', fg = 'palette.comment' },
            OilVcsStatusModified = { bg = 'none', fg = 'palette.yellow' },
            OilVcsStatusRenamed = { bg = 'none', fg = 'palette.blue' },
            OilVcsStatusUntracked = { bg = 'none', fg = 'palette.magenta' },
            OilVcsStatusUpstreamAdded = { bg = 'none', 'palette.green' },
            OilVcsStatusUpstreamCopied = { bg = 'none', 'palette.orange' },
            OilVcsStatusUpstreamDeleted = { bg = 'none', 'palette.red' },
            OilVcsStatusUpstreamIgnored = { bg = 'none', 'palette.comment' },
            OilVcsStatusUpstreamModified = { bg = 'none', 'palette.yellow' },
            OilVcsStatusUpstreamRenamed = { bg = 'none', 'palette.blue' },
            OilVcsStatusUpstreamUntracked = { bg = 'none', 'palette.magenta' },
        },
    },
})

vim.cmd('colorscheme carbonfox')
