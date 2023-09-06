return {
  'echasnovski/mini.tabline',
  event = "BufEnter",
  version = '*',
  config = function()
    require("mini.tabline").setup()
  end,
}
