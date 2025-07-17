return {
    'NeogitOrg/neogit',
    dependencies = {
        'nvim-lua/plenary.nvim', -- required
    },
    lazy = false,
    config = function()
        require('neogit').setup({
            disable_context_highlighting = true,
            mappings = {
                popup = {
                    ['P'] = 'PushPopup',
                    ['p'] = 'PushPopup',
                    ['F'] = 'PullPopup',
                },
            },
        })
    end,
}
