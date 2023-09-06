return {
  "kosayoda/nvim-lightbulb",
  config = function()
    require("nvim-lightbulb").setup({
      autocmd = { enabled = true },
      link_highlights = false,
      sign = {
        enabled = true,
        -- Text to show in the sign column.
        -- Must be between 1-2 characters.
        text = "",
        -- Highlight group to highlight the sign column text.
        hl = "DiagnosticSignHint",
      },
    })
  end
}
