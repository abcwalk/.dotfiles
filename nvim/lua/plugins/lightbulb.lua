require('nvim-lightbulb').setup({
  sign = {
    enabled = true,
    priority = 10,
  },
})

vim.fn.sign_define("LightBulbSign", { text = "" })

vim.cmd [[
augroup lightbulb
  au!
  autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
augroup end
]]
