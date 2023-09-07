return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  event = { "BufReadPost", "BufNewFile" },
  cmd = { "TSUpdateSync" },
  opts = {
    ensure_installed = {
      "typescript",
      "javascript",
      "css",
      "html",
      "sql",
      "markdown",
      "json5",
      "jsdoc",
      "java",
      "lua",
      "vimdoc",
      "vim",
      "query",
      "c",
      "comment",
      "regex",
    },
    auto_install = true,
    indent = { enable = true },
    highlight = { enable = true },
  }
}
