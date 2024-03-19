vim.api.nvim_create_user_command('Swenv', function()
    require('swenv.api').pick_venv()
end, {})

vim.api.nvim_create_user_command('Dos2Unix', function()
    vim.cmd([[%s/\r//]])
end, {})
