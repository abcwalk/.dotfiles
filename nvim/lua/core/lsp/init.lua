local completion_engine = "blink-cmp"

local M = {
	"neovim/nvim-lspconfig",
	event = "BufReadPre",
	dependencies = {
		(completion_engine == "nvim-cmp" and "hrsh7th/cmp-nvim-lsp" or null),
		(completion_engine == "blink-cmp" and "saghen/blink.cmp" or null),
	},
	pin = true,
	opts = {
		inlay_hints = { enabled = true },
	},
}

function M.config()
	require("mason")
	require("user.plugins.lsp.diagnostics").setup()

	local function on_attach(client, bufnr)
		require("user.plugins.lsp.keys").setup(client, bufnr)
		-- metalelf0 customization - force definitionProvider to true to work around dynamicRegistration for solargraph
		client.server_capabilities.definitionProvider = true

		-- disable semanticTokensProvider cause it's giving me headache with ruby_ls
		client.server_capabilities.semanticTokensProvider = nil
	end

	---@type lspconfig.options
	local servers = {
		ansiblels = {},
		bashls = {},
		clangd = {},
		cssls = {},
		dockerls = {},
		tsserver = {},
		-- svelte = {},
		eslint = {},
		html = {},
		-- elixirls = {
		-- 	cmd = { os.getenv("HOME") .. "/.local/share/nvim/mason/bin/elixir-ls" },
		-- },
		jsonls = {
			on_new_config = function(new_config)
				new_config.settings.json.schemas = new_config.settings.json.schemas or {}
				vim.list_extend(new_config.settings.json.schemas, require("schemastore").json.schemas())
			end,
			settings = {
				json = {
					format = {
						enable = true,
					},
					validate = {
						enable = true,
					},
				},
			},
		},
		-- pyright = {},
        basedpyright = {
                settings = {
                    basedpyright = {
                        disableOrganizeImports = true,
                        analysis = {
                            diagnosticMode = 'openFilesOnly',
                            typeCheckingMode = 'off',
                            autoSearchPaths = true,
                            exclude = {
                                '/.cache',
                                '/.mypy_cache',
                                '/.pytest_cache',
                                '/.ruff_cache',
                                '/.venv',
                                '/venv',
                                '/pycache',
                                '/dist',
                                '/node_modules',
                            },
                        },
                    },
                },
            },
		-- rust_analyzer = {
		-- 	settings = {
		-- 		["rust-analyzer"] = {
		-- 			cargo = { allFeatures = true },
		-- 			checkOnSave = {
		-- 				command = "clippy",
		-- 				extraArgs = { "--no-deps" },
		-- 			},
		-- 		},
		-- 	},
		-- },
		yamlls = {},
		lua_ls = {
			single_file_support = true,
			settings = {
				Lua = {
					workspace = {
						checkThirdParty = false,
					},
					completion = {
						workspaceWord = true,
						callSnippet = "Both",
					},
					misc = {
						parameters = {
							"--log-level=trace",
						},
					},
					diagnostics = {
						-- enable = false,
						globals = {
							"vim",
						},
						groupSeverity = {
							strong = "Warning",
							strict = "Warning",
						},
						groupFileStatus = {
							["ambiguity"] = "Opened",
							["await"] = "Opened",
							["codestyle"] = "None",
							["duplicate"] = "Opened",
							["global"] = "Opened",
							["luadoc"] = "Opened",
							["redefined"] = "Opened",
							["strict"] = "Opened",
							["strong"] = "Opened",
							["type-check"] = "Opened",
							["unbalanced"] = "Opened",
							["unused"] = "Opened",
						},
						unusedLocalExclude = { "_*" },
					},
					format = {
						enable = false,
						defaultConfig = {
							indent_style = "space",
							indent_size = "2",
							continuation_indent_size = "2",
						},
					},
					hint = { enable = true },
				},
			},
		},
		-- teal_ls = {},
		vimls = {},
		-- solargraph = {
		-- 	root_dir = require("lspconfig").util.root_pattern(".git", "Gemfile", vim.fn.getcwd()),
		-- 	settings = {
		-- 		solargraph = {
		-- 			hint = { enable = true },
		-- 			diagnostics = true,
		-- 			completion = true,
		-- 			useBundler = true,
		-- 		},
		-- 	},
		-- },
		-- ruby_lsp = {
		-- 	default_config = {
		-- 		cmd = { "bundle", "exec", "ruby-lsp" },
		-- 		filetypes = { "ruby" },
		-- 		root_dir = require("lspconfig").util.root_pattern("Gemfile", ".git", vim.fn.getcwd()),
		-- 		init_options = {
		-- 			formatter = "auto",
		-- 			single_file_support = true,
		-- 		},
		-- 		settings = {},
		-- 	},
		-- 	commands = {
		-- 		FormatRuby = {
		-- 			function()
		-- 				vim.lsp.buf.format({
		-- 					name = "ruby_lsp",
		-- 					async = true,
		-- 				})
		-- 			end,
		-- 			description = "Format using ruby-lsp",
		-- 		},
		-- 	},
		-- },
	}

	local capabilities = vim.lsp.protocol.make_client_capabilities()

	if completion_engine == "nvim-cmp" then
		capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
	elseif completion_engine == "blink-cmp" then
		capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)
	end

	capabilities.textDocument.foldingRange = {
		dynamicRegistration = false,
		lineFoldingOnly = true,
	}

	---@type _.lspconfig.options
	local options = {
		on_attach = on_attach,
		capabilities = capabilities,
		flags = {
			debounce_text_changes = 150,
		},
	}

	for server, opts in pairs(servers) do
		opts = vim.tbl_deep_extend("force", {}, options, opts or {})
		require("lspconfig")[server].setup(opts)
	end
end

return M
