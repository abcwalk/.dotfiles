local status_ok, saga = pcall(require, 'lspsaga')
if not status_ok then
  return
end

saga.setup({
  preview = {
    lines_above = 0,
    lines_below = 10,
  },
  scroll_preview = {
    scroll_down = '<C-d>',
    scroll_up = '<C-u>',
  },
  finder = {
    default = 'imp+def+ref',
    keys = {
      toggle_or_open = '<CR>',
      vsplit = '<C-v>',
      split = '<C-x>',
      quit = 'q',
      -- close = 'q',
    },
  },
  lightbulb = {
    enable = false,
  },
  symbol_in_winbar = {
    enable = true,
    -- separator = '  ',
    hide_keyword = true,
    show_file = false,
  },
  implement = {
    enable = true,
    sign = true,
    virtual_text = true,
    -- priority = 100,
  },
  request_timeout = 2500,
  definition = {
    keys = {
      edit = '<CR>',
      vsplit = '<C-v>',
      split = '<C-x>',
      quit = 'q',
    },
  },
  diagnostic = {
    -- custom_fix = 'Code Actions',
    on_insert = false,
    on_insert_follow = false,
    max_width = 0.4,
    max_show_width = 0.7,
    show_code_action = false,
    keys = {
      quit = { 'q', '<ESC>' },
    },
  },
  rename = {
    in_select = false,
    keys = {
      quit = '<C-c>',
    },
  },
  ui = {
    theme = 'round',
    border = 'rounded',
    code_action = '',
    diagnostic = '',
    devicon = true,
    title = true,
  },
})

--Gutter icons
local signs = {
  Error = '',
  Warn = '',
  Hint = '',
  Info = '',
  Question = '',
}

for type, icon in pairs(signs) do
  local hl = 'DiagnosticSign' .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
