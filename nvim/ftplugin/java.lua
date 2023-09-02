local _jdtls, jdtls = pcall(require, "jdtls")
local coq = require "coq"

if not _jdtls then
  print("nvim-jdtls not installed")
  return
end

local function on_init(client)
  if client.config.settings then
    client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
  end
end

local root_markers = { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle", ".project" }
-- local root_dir = require("jdtls.setup").find_root(root_markers)
local root_dir = vim.fs.dirname(vim.fs.find({ "gradlew", ".git", "mvnw" }, { upward = true })[1])
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
local workspace_dir = vim.fn.stdpath "data" .. "/site/java/workspace-root/" .. project_name
-- Set proper Java executable
-- local java_cmd = "/usr/bin/java"

-- local bundles = {
--   mason_registry.get_package("java-debug-adapter"):get_install_path()
--   .. "/extension/server/com.microsoft.java.debug.plugin-*.jar",
-- }
-- vim.list_extend(
--   bundles,
--   vim.split(
--     vim.fn.glob(mason_registry.get_package("java-test"):get_install_path() .. "/extension/server/*.jar"),
--     "\n"
--   )
-- )

local on_attach = function(client, bufnr)
  if client.name == "jdtls" then
    jdtls = require("jdtls")
    -- jdtls.setup_dap({ hotcodereplace = "auto" })
    jdtls.setup.add_commands()
    -- Auto-detect main and setup dap config
    -- require("jdtls.dap").setup_dap_main_class_configs({
    --   config_overrides = {
    --     vmArgs = "-Dspring.profiles.active=local",
    --   },
    -- })
  end
end

local config = {
  cmd = {
    "java",
    "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    "-Dosgi.bundles.defaultStartLevel=4",
    "-Declipse.product=org.eclipse.jdt.ls.core.product",
    "-Dlog.protocol=true",
    "-Dlog.level=ALL",
    "-Xms1g",
    "--add-modules=ALL-SYSTEM",
    "--add-opens",
    "java.base/java.util=ALL-UNNAMED",
    "--add-opens",
    "java.base/java.lang=ALL-UNNAMED",
    "-jar",
    vim.fn.expand "$MASON/share/jdtls/plugins/org.eclipse.equinox.launcher.jar",
    "-configuration",
    vim.fn.expand "$MASON/share/jdtls/config",
    "-data",
    workspace_dir,
  },
  -- flags = {
  --   debounce_text_changes = 150,
  --   allow_incremental_sync = true,
  -- - },
  root_dir = root_dir,

  on_init = on_init,
  -- init_options = {
  --   bundles = bundles,
  -- },
  on_attach = on_attach,
  settings = {
    java = {
      eclipse = {
        downloadSources = true,
      },
      configuration = {
        updateBuildConfiguration = "interactive",
      },
      maven = {
        downloadSources = true,
      },
      implementationsCodeLens = {
        enabled = true,
      },
      referencesCodeLens = {
        enabled = false,
      },
      references = {
        includeDecompiledSources = true,
      },
      format = {
        enabled = true,
      },
      signatureHelp = {
        enabled = true,
      },
      saveActions = {
        organizeImports = true,
      },
      completion = {
        -- maxResults = 20,
        favoriteStaticMembers = {
          "org.hamcrest.MatcherAssert.assertThat",
          "org.hamcrest.Matchers.*",
          "org.hamcrest.CoreMatchers.*",
          "org.junit.jupiter.api.Assertions.*",
          "java.util.Objects.requireNonNull",
          "java.util.Objects.requireNonNullElse",
          "org.mockito.Mockito.*",
        },
      },
      sources = {
        organizeImports = {
          starThreshold = 9999,
          staticStarThreshold = 9999,
        },
      },
      codeGeneration = {
        toString = {
          template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
        },
        useblocks = true,
      },
    },
  },
}

jdtls.start_or_attach(config)
