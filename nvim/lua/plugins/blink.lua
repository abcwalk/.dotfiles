return {
    { 'L3MON4D3/LuaSnip', keys = {} },
    {
        'saghen/blink.cmp',
        dependencies = {
            'rafamadriz/friendly-snippets',
        },
        version = '*',
        opts = {
            snippets = { preset = 'luasnip' },
            signature = { enabled = true, window = { border = 'rounded' } },
            appearance = {
                use_nvim_cmp_as_default = true,
                nerd_font_variant = 'normal',
            },
            sources = {
                default = { 'lsp', 'path', 'snippets', 'buffer' },
                providers = {
                    cmdline = {
                        min_keyword_length = 2,
                    },
                },
            },
            keymap = {
                ['<C-c>'] = { 'hide', 'fallback' },
                ['<CR>'] = { 'select_and_accept', 'fallback' },
                ['<Up>'] = { 'select_prev', 'fallback' },
                ['<Down>'] = { 'select_next', 'fallback' },
                ['<Tab>'] = {
                    function(cmp)
                        if cmp.snippet_active() then
                            return cmp.snippet_forward()
                        end
                    end,
                    'select_next',
                    'fallback',
                },
                ['<S-Tab>'] = {
                    function(cmp)
                        if cmp.snippet_active() then
                            return cmp.snippet_backward()
                        end
                    end,
                    'select_prev',
                    'fallback',
                },
                ['<C-k>'] = { 'show_signature', 'hide_signature', 'fallback' },
            },
            cmdline = {
                enabled = false,
                completion = { menu = { auto_show = true } },
                keymap = {
                    ['<CR>'] = { 'accept_and_enter', 'fallback' },
                },
            },
            completion = {
                menu = {
                    border = 'rounded',
                    scrolloff = 1,
                    scrollbar = false,
                    draw = {
                        columns = {
                            { 'kind_icon' },
                            { 'label', 'label_description', gap = 1 },
                            { 'kind' },
                            -- { 'source_name' },
                        },
                    },
                },
                documentation = {
                    window = {
                        border = 'rounded',
                        scrollbar = false,
                        winhighlight = 'Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,EndOfBuffer:BlinkCmpDoc',
                    },
                    auto_show = true,
                },
            },
        },
    },
}
