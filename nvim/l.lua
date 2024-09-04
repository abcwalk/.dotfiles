require("conform").setup({
	formatters = {
		injected = {
			options = {
				ignore_errors = false,
				lang_to_formatters = {
					sql = { "sqlfluff" },
				},
				lang_to_ext = {
					sql = "sql",
				},
			},
		},
		sqlfluff = {
			command = "sqlfluff",
			args = {
				"fix",
				"--dialect",
				"sqlite",
				"--disable-progress-bar",
				"-f",
				"-n",
				"-",
			},
			stdin = true,
		},
	},
	formatters_by_ft = {
		lua = { "stylua" },
		-- python = { "isort", "black" },
		python = { "ruff_format", "black", "injected" },
		nix = { "alejandra" },
		sql = { "sqlfluff", "injected" },
	},
})
require("conform").formatters.injected = {
	options = {
		ignore_errors = false,
		lang_to_formatters = {
			sql = { "sqlfluff" },
		},
	},
}
