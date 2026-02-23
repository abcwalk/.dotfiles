return {
    'linux-cultist/venv-selector.nvim',
    lazy = false,
    keys = {
        { ',v', '<cmd>VenvSelect<cr>' },
    },
    opts = {
        options = {
            notify_user_on_venv_activation = true,
        },
        search = {
            my_venvs = {
                command = 'fd python3.10 ~/venvs',
            },
            local_venvs = {
                command = 'fd -H -t d "^\\.venv$" --max-depth 2',
            },
        },
    },
}
