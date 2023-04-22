local M = {}

function M.setup(servers)
  require("mason").setup({
    ui = {
      icons = {
        package_installed = "✓",
        package_pending = "➜",
        package_uninstalled = "✗"
      }
    }
  })

  require("mason-tool-installer").setup {
    ensure_installed = {
      "clang-format",
      "cpplint",
      "jsonlint",
      "shellcheck",
      "shfmt",
      "stylua",
      "taplo",
    },
    auto_update = false,
    run_on_start = true,
  }

  require("mason-lspconfig").setup {
    ensure_installed = servers,
    automatic_installation = false,
  }
end

return M
