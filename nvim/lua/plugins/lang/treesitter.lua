local status_ok, configs = pcall(require, 'nvim-treesitter.configs')
if not status_ok then
    return
end

local installed_parsers = require('plugins.list').ts_parsers

configs.setup({
    -- ensure_installed = installed_parsers,
    sync_install = false,
    ignore_install = {},
    auto_install = true,
    highlight = {
        enable = true,
        disable = {},
        additional_vim_regex_highlighting = false,
    },
})
