return {
  "nvim-treesitter/nvim-treesitter",
  config = function()
    require("nvim-treesitter.configs").setup {
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
      highlight = {
        enable = true,
      },
    }
  end,
}
