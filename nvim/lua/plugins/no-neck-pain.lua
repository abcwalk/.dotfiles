return {
  "shortcuts/no-neck-pain.nvim",
  version = "*",
  config = function()
    require("no-neck-pain").setup({
      width = 80,
      buffers = {
        right = {
          enabled = false,
        },
        left = {
          colors = {
            blend = -0.2,
          },
          scratchPad = {
            enabled = true,
            location = nil,
          },
          bo = {
            filetype = "md"
          },
        },
      },
    })
  end
}
