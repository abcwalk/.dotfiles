local status_ok, telescope = pcall(require, 'telescope')
if not status_ok then
    return
end

telescope.setup({
    pickers = {
        find_files = {
            theme = 'ivy',
        },
        live_grep = {
            theme = 'ivy',
        },
        grep_string = {
            theme = 'ivy',
        },
    },
})
-- require('telescope').load_extension('projects')
