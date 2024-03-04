local status_ok, saga = pcall(require, 'lspsaga')
if not status_ok then
  return
end

saga.setup({
  finder = {
    default = 'imp+def+ref',
    keys = {
      toggle_or_open = '<CR>',
      vsplit = '<C-v>',
      split = '<C-x>',
      quit = '<ESC>',
      close = 'q',
    },
  },
  lightbulb = {
    enable = false,
  },
  symbol_in_winbar = {
    enable = false,     -- showing symbols in feline
    separator = '  ',
    hide_keyword = true,
    show_file = false,
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
    custom_fix = 'Code Actions',
    on_insert = false,
    on_insert_follow = false,
    max_width = 0.4,
    max_show_width = 0.7,
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
