local status_ok, hlslens = pcall(require, 'hlslens')
if not status_ok then
    return
end

local opts = { noremap = true, silent = true }

hlslens.setup({
    calm_down = true,
})
