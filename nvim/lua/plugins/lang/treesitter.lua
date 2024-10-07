local status_ok, configs = pcall(require, 'nvim-treesitter.configs')
if not status_ok then
    return
end

local ts_parsers = {
    'bash',
    'css',
    'comment',
    'gitcommit',
    'go',
    'gosum',
    'gomod',
    'html',
    'java',
    'javascript',
    'json',
    'requirements',
    'lua',
    'markdown',
    'markdown_inline',
    'python',
    'rust',
    'typescript',
    'vim',
    'vimdoc',
    'yaml',
}

configs.setup({
    ensure_installed = ts_parsers,
    auto_install = true,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
})
