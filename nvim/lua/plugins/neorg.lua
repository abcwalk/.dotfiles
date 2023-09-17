return {
  {
    "nvim-neorg/neorg",
    build = ":Neorg sync-parsers",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("neorg").setup {
        load = {
          ["core.defaults"] = {}, -- Loads default behaviour
          ["core.keybinds"] = {
            config = {
              hook = function(keybinds)
                -- Creates a new .norg file to take notes in
                keybinds.remap_event("norg", "n", "nn", "core.dirman.new.note")
                -- Show toc-page
                vim.keymap.set("n", "nt", ":Neorg toc<CR>", { noremap = true, silent = true })
                -- Marks the task under the cursor as "ambiguous"
                keybinds.remap_event("norg", "n", "ta", "core.qol.todo_items.todo.task_ambiguous")
                -- Marks the task under the cursor as "important"
                keybinds.remap_event("norg", "n", "ti", "core.qol.todo_items.todo.task_important")
                -- Marks the task under the cursor as "recurring"
                keybinds.remap_event("norg", "n", "tr", "core.qol.todo_items.todo.task_recurring")
                -- Marks the task under the cursor as "cancelled"
                keybinds.remap_event("norg", "n", "tc", "core.qol.todo_items.todo.task_cancelled")
                -- Marks the task under the cursor as "on_hold"
                keybinds.remap_event("norg", "n", "th", "core.qol.todo_items.todo.task_on_hold")
                -- Marks the task under the cursor as "done"
                keybinds.remap_event("norg", "n", "ti", "core.qol.todo_items.todo.task_done")
                -- Marks the task under the cursor as "undone"
                keybinds.remap_event("norg", "n", "tr", "core.qol.todo_items.todo.task_undone")
                -- Marks the task under the cursor as "pending"
                keybinds.remap_event("norg", "n", "tc", "core.qol.todo_items.todo.task_pending")
              end,
            }
          },
          ["core.concealer"] = {}, -- Adds pretty icons to your documents
          ["core.dirman"] = {      -- Manages Neorg workspaces
            config = {
              workspaces = {
                notes = "~/notes",
              },
            },
          },
        },
      }
    end,
  },
}
