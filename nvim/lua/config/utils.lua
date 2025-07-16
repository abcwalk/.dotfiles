local M = {}

-- Copy the current file path
M.copy_file_path = function()
    local current_file = vim.fn.expand('%:p')
    vim.fn.setreg('+', current_file)
    print('Copied to clipboard: ' .. current_file)
end

return M
