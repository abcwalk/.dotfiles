local null_ls = require "null-ls"
local eslintd_opts = { prefer_local = true }

null_ls.setup {
  sources = {
    null_ls.builtins.diagnostics.eslint_d.with(eslintd_opts),
    null_ls.builtins.code_actions.eslint_d.with(eslintd_opts),
    null_ls.builtins.code_actions.gitsigns,
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.formatting.shfmt,
  },
}
