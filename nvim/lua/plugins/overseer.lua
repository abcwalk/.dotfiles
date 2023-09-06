return {
  'stevearc/overseer.nvim',
  config = function()
    require('overseer').setup({
      templates = { "java_build" },
      task_list = {
        direction = "bottom",
        max_height = 15,
        default_detail = 1,
        bindings = { ["q"] = function() vim.cmd("OverseerClose") end },
      },
    })
  end
}
