return {
  "shortcuts/no-neck-pain.nvim",
  version = "*",
  config = function()
    require("no-neck-pain").setup({
      autocmds = {
        enableOnVimEnter = true,
        enableOnTabEnter = true,
      },
      buffers = {
        right = {
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
