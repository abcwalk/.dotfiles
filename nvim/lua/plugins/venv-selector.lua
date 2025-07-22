return {
    'linux-cultist/venv-selector.nvim',
    lazy = false,
    branch = 'regexp',
    keys = {
        { ',v', '<cmd>VenvSelect<cr>' },
    },
    opts = {
        search = {
            my_venvs = {
                command = 'fd python3.10 ~/venvs',
            },
        },
    },
}
