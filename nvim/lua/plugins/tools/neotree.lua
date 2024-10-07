local status, neotree = pcall(require, 'neotree')
if not status then
    return
end

neotree.setup({
    filesystem = {
        hijack_netrw_behavior = 'open_current',
    },
})
