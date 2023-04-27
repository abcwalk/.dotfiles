local dap, dapui, dap_vscode_js = require("dap"), require("dapui"), require("dap-vscode-js")

dap.set_log_level('INFO')

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
--WARNING Autoclose prevent
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end

dap_vscode_js.setup({
  adapters = { 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost' },
})

for _, language in ipairs({ "typescript", "javascript" }) do
  dap.configurations[language] = {
    {
      name = "Launch file",
      request = "launch",
      type = "pwa-node",
      program = "${file}",
      cwd = "${workspaceFolder}",
    },
    {
      name = "Launch file (with Logger)",
      type = "pwa-node",
      request = "launch",
      program = "${file}",
      cwd = "${workspaceFolder}",
    },

    -- {
    --   type = "pwa-node",
    --   request = "attach",
    --   name = "Attach",
    --   processId = require 'dap.utils'.pick_process,
    --   cwd = "${workspaceFolder}",
    -- },
  }
end

-- dap.adapters.go = {
--   type = "server",
--   port = "${port}",
--   executable = {
--     command = vim.fn.stdpath("data") .. '/mason/bin/dlv',
--     args = { "dap", "-l", "127.0.0.1:${port}" },
--   },
-- }
