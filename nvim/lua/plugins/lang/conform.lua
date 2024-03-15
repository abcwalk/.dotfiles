local status_ok, conform = pcall(require, 'conform')
if not status_ok then
    return
end

conform.setup({
  formatters_by_ft = {
    lua = { "stylua" },
    go = { "goimports", "gofumpt", "goimports_reviser", "golines" },
    python = { "black" , "isort"},
    ["*"] = { "codespell" },
    ["_"] = { "trim_whitespace" },
  },
    format_on_save = { timeout_ms = 500, lsp_fallback = true },
    notify_on_error = true,
})
