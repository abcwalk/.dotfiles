vim.cmd "colorscheme coal"

local Main_color = "#80a6ed"
local Type_color = "#4EC9B0"
local Function_color = "#93abbd"
local Red_color = "#960019"
local Yellow_color = "#afa100"
local Green_color = "#369432"
local Gray_color = "#7a7a7a"

--Main
vim.api.nvim_set_hl(0, "CursorLine", { bg = "#121212" })
vim.api.nvim_set_hl(0, "Normal", { bg = "black" })
vim.api.nvim_set_hl(0, "NormalNC", { bg = "black" })
vim.api.nvim_set_hl(0, "LineNr", { bg = "black", fg = "#A5A5A5" })
vim.api.nvim_set_hl(0, "CursorLineNR", { fg = "#454545" })
vim.api.nvim_set_hl(0, "CursorLineNR", { fg = "#454545" })
vim.api.nvim_set_hl(0, "NonText", { bg = "black", fg = Gray_color })
vim.api.nvim_set_hl(0, "SignColumn", { bg = "black" })
vim.api.nvim_set_hl(0, "StatusLine", { fg = "#c6c6c6" })
vim.api.nvim_set_hl(0, "MsgArea", { fg = Gray_color })
vim.api.nvim_set_hl(0, "Visual", { bg = "#0b1e33" })
vim.api.nvim_set_hl(0, "IncSearch", { link = "@text.todo" })
vim.api.nvim_set_hl(0, "Substitute", { link = "@text.todo" })
vim.api.nvim_set_hl(0, "Search", { link = "@text.todo" })
vim.api.nvim_set_hl(0, "NormalFloat", { fg = Main_color, bg = "black" })
vim.api.nvim_set_hl(0, "Pmenu", { fg = Main_color, bg = "black" })
vim.api.nvim_set_hl(0, "PmenuSel", { bg = "#0b1e33" })
vim.api.nvim_set_hl(0, "PmenuThumb", { bg = Gray_color })
vim.api.nvim_set_hl(0, "ModeMsg", { fg = Main_color, bg = "black" })
vim.api.nvim_set_hl(0, "TabLineSel", { fg = Main_color, bg = "black" })
vim.api.nvim_set_hl(0, "WarningMsg", { fg = Main_color, bg = "black" })
vim.api.nvim_set_hl(0, "FloatBorder", { fg = Function_color, bg = "black" })
vim.api.nvim_set_hl(0, "MatchParen", { bg = "black", fg = Red_color, underline = true })
vim.api.nvim_set_hl(0, "Title", { link = "Pmenu" })
vim.api.nvim_set_hl(0, "Comment", { fg = "#454545", italic = true })

--To-do
vim.api.nvim_set_hl(0, "Todo", { fg = "#4BA8FF", bg = "#0b1e33" })
vim.api.nvim_set_hl(0, "@text.todo", { link = "Todo" })
vim.api.nvim_set_hl(0, "@text.note", { fg = "#FF57FF", bg = "#1b0e23" })
vim.api.nvim_set_hl(0, "@text.warning", { fg = "#FF9900", bg = "#392a13" })
vim.api.nvim_set_hl(0, "@text.danger", { fg = "#FF8B64", bg = "#391a13" })

--TODO
--NOTE
--WARNING
--BUG

--URI
vim.api.nvim_set_hl(0, "@text.uri", { fg = Main_color, bg = "black", underline = true })

--Markdown todo checkbox
vim.api.nvim_set_hl(0, "@unchecked_list_item", { fg = "#F8F8F2" })
vim.api.nvim_set_hl(0, "@checked_list_item", { fg = Gray_color, strikethrough = true })
vim.api.nvim_set_hl(0, "@text.todo.unchecked", { link = "@unchecked_list_item" })
vim.api.nvim_set_hl(0, "@text.todo.checked", { link = "@checked_list_item" })

--Semantic
vim.api.nvim_set_hl(0, "@keyword", { fg = Main_color })
vim.api.nvim_set_hl(0, "@function", { fg = Function_color })
vim.api.nvim_set_hl(0, "@method", { link = "@function" })
vim.api.nvim_set_hl(0, "@constructor", { link = "@function" })
vim.api.nvim_set_hl(0, "@property", { fg = Main_color })
vim.api.nvim_set_hl(0, "@conditional", { fg = Gray_color })
vim.api.nvim_set_hl(0, "@exception", { fg = Red_color })
vim.api.nvim_set_hl(0, "@repeat", { link = "@conditional" })
vim.api.nvim_set_hl(0, "@type", { fg = Type_color })
vim.api.nvim_set_hl(0, "@label", { link = "@keyword" })
vim.api.nvim_set_hl(0, "@constant", { link = "@type" })
vim.api.nvim_set_hl(0, "@variable.builtin", { fg = Gray_color })
vim.api.nvim_set_hl(0, "Number", { fg = "#225a99" })
vim.api.nvim_set_hl(0, "String", { fg = Green_color })
vim.api.nvim_set_hl(0, "Error", { fg = Red_color })
vim.api.nvim_set_hl(0, "Boolean", { link = "String" })
vim.api.nvim_set_hl(0, "Float", { link = "Number" })
vim.api.nvim_set_hl(0, "SpecialChar", { fg = "#967bb6" })          --\n \r RegExp
vim.api.nvim_set_hl(0, "Tag", { link = "SpecialChar" })
vim.api.nvim_set_hl(0, "SpecialComment", { link = "SpecialChar" }) --?
vim.api.nvim_set_hl(0, "StorageClass", { link = "@keyword" })
vim.api.nvim_set_hl(0, "Function", { link = "@function" })

--Languanges
vim.api.nvim_set_hl(0, "@css.property_name", { fg = Gray_color })

vim.api.nvim_set_hl(0, "@c.primitive_type", { link = "@keyword" })
vim.api.nvim_set_hl(0, "@c.function_identifier", { link = "@function" })

-- *Type    int, long, char, etc.
--  StorageClass  static, register, volatile, etc.
--  Structure  struct, union, enum, etc.
--  Typedef  A typedef

-- *Special  any special symbol
--   SpecialChar  special character in a constant
--   Tag    you can use CTRL-] on this
--   Delimiter  character that needs attention
--   SpecialComment  special things inside a comment
--   Debug    debugging statements

--Diagnostic
vim.api.nvim_set_hl(0, "DiagnosticSignError", { fg = Red_color })
vim.api.nvim_set_hl(0, "DiagnosticSignWarn", { fg = Yellow_color })
vim.api.nvim_set_hl(0, "DiagnosticSignHint", { fg = Gray_color })
vim.api.nvim_set_hl(0, "DiagnosticSignInfo", { fg = Main_color })
vim.api.nvim_set_hl(0, "DiagnosticError", { link = "DiagnosticSignError" })
vim.api.nvim_set_hl(0, "DiagnosticWarn", { link = "DiagnosticSignWarn" })
vim.api.nvim_set_hl(0, "DiagnosticHint", { link = "DiagnosticSignHint" })
vim.api.nvim_set_hl(0, "DiagnosticInfo", { link = "DiagnosticSignInfo" })
vim.api.nvim_set_hl(0, "DiffAdd", { bg = "#122f2f" })
vim.api.nvim_set_hl(0, "DiffAdded", { link = "@text.todo" })
vim.api.nvim_set_hl(0, "DiffChange", { bg = "#222a39" })
vim.api.nvim_set_hl(0, "DiffChanged", { fg = Main_color })
vim.api.nvim_set_hl(0, "DiffDelete", { bg = "#361c28" })
vim.api.nvim_set_hl(0, "DiffRemoved", { fg = Red_color })
vim.api.nvim_set_hl(0, "DiffText", { link = "@text.todo" })

--Git
vim.api.nvim_set_hl(0, "GitSignsAdd", { fg = Green_color })
vim.api.nvim_set_hl(0, "GitSignsChange", { fg = Yellow_color })
vim.api.nvim_set_hl(0, "GitSignsDelete", { fg = Red_color })

--Plugins
vim.api.nvim_set_hl(0, "ZenBg", { bg = "black" })

vim.api.nvim_set_hl(0, "FloatermBorder", { link = "FloatBorder" })

vim.api.nvim_set_hl(0, "NeoTreeGitModified", { fg = Main_color })
vim.api.nvim_set_hl(0, "NeoTreeGitUnstaged", { fg = Main_color })
vim.api.nvim_set_hl(0, "NeoTreeModified", { fg = Main_color })
vim.api.nvim_set_hl(0, "NeoTreeTitleBar", { fg = Main_color })
vim.api.nvim_set_hl(0, "NeoTreeGitUntracked", { fg = "#7a7a7a" })
vim.api.nvim_set_hl(0, "NeoTreeFloatBorder", { fg = "black" })
vim.api.nvim_set_hl(0, "NeoTreeRootName", { fg = "#4BA8FF", bold = true, italic = true })

vim.api.nvim_set_hl(0, "MiniTablineModifiedCurrent", { fg = Yellow_color })
vim.api.nvim_set_hl(0, "MiniTablineModifiedHidden", { fg = "#785705" })

-- .zshrc >> export FZF_DEFAULT_OPTS="--color=bg+:#0b1e33,fg+:#4BA8FF,gutter:-1"
vim.api.nvim_set_hl(0, "FzfLuaCursorLine", { link = "@text.todo" })

--Scheme icon
vim.api.nvim_set_hl(0, "DevIconScheme", { fg = Red_color })

--TreeSitter CSS
--WARNING i dont know how it works
-- require("vim.treesitter.query").set_query("css", "highlights", "(class_selector) @keyword")-- require("vim.treesitter.query").set_query("css", "highlights", "(class_selector) @keyword")
