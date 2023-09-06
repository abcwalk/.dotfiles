return {
  "gennaro-tedesco/nvim-possession",
  dependencies = {
    "ibhagwan/fzf-lua",
  },
  config = function()
    local possession = require "nvim-possession"
    possession.setup()
    vim.keymap.set("n", "<Bslash><Bslash>", function()
      possession.list()
    end)
    vim.keymap.set("n", "<Space>sn", function()
      possession.new()
    end)
    vim.keymap.set("n", "<Space>su", function()
      possession.update()
    end)
  end,
}
