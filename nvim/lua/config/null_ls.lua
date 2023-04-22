local null_ls = require "null-ls"
local eslintd_opts = { prefer_local = true }

null_ls.setup {
  sources = {
    null_ls.builtins.diagnostics.eslint_d.with(eslintd_opts),
    null_ls.builtins.code_actions.eslint_d.with(eslintd_opts),
    null_ls.builtins.formatting.stylua.with {
      extra_args = { "--config-path", vim.fn.expand "~/.config/stylua.toml" },
    },
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.code_actions.shellcheck,
    null_ls.builtins.formatting.shfmt,
    null_ls.builtins.diagnostics.todo_comments,
    null_ls.builtins.formatting.clang_format,
    null_ls.builtins.diagnostics.cpplint,
  },
}
