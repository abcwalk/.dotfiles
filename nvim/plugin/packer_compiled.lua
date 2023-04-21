-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

_G._packer = _G._packer or {}
_G._packer.inside_compile = true

local time
local profile_info
local should_profile = false
if should_profile then
  local hrtime = vim.loop.hrtime
  profile_info = {}
  time = function(chunk, start)
    if start then
      profile_info[chunk] = hrtime()
    else
      profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
    end
  end
else
  time = function(chunk, start) end
end

local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end
  if threshold then
    table.insert(results, '(Only showing plugins that took longer than ' .. threshold .. ' ms ' .. 'to load)')
  end

  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/nonh/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/nonh/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/nonh/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/nonh/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/nonh/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["Comment.nvim"] = {
    config = { "\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\fComment\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/Comment.nvim",
    url = "https://github.com/numToStr/Comment.nvim"
  },
  ["FixCursorHold.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/FixCursorHold.nvim",
    url = "https://github.com/antoinemadec/FixCursorHold.nvim"
  },
  ["alternate-toggler"] = {
    config = { "\27LJ\2\nb\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\15alternates\1\0\0\1\0\1\a==\a!=\nsetup\22alternate-toggler\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/opt/alternate-toggler",
    url = "https://github.com/rmagatti/alternate-toggler"
  },
  ["coal.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/coal.nvim",
    url = "https://github.com/cranberry-clockworks/coal.nvim"
  },
  ["coq.artifacts"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/coq.artifacts",
    url = "https://github.com/ms-jpq/coq.artifacts"
  },
  coq_nvim = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/coq_nvim",
    url = "https://github.com/ms-jpq/coq_nvim"
  },
  ["diffview.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/diffview.nvim",
    url = "https://github.com/sindrets/diffview.nvim"
  },
  ["dressing.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/dressing.nvim",
    url = "https://github.com/stevearc/dressing.nvim"
  },
  ["express_line.nvim"] = {
    config = { "\27LJ\2\nJ\0\2\6\1\2\0\f-\2\0\0009\2\0\2\18\4\0\0\18\5\1\0B\2\3\2\15\0\2\0X\3\4Ä'\3\1\0\18\4\2\0&\3\4\3L\3\2\0K\0\1\0\0\0\n%=Óú•\15git_branchç\1\1\2\v\2\a\0\0194\2\0\0006\3\0\0009\3\1\3\18\5\2\0-\6\0\0009\6\2\6B\3\3\0016\3\0\0009\3\1\3\18\5\2\0-\6\1\0009\6\3\6'\b\4\0'\t\5\0003\n\6\0B\6\4\0A\3\1\0012\0\0ÄL\2\2\0\0¿\1¿\0\rBufEnter\18el_git_branch\16buf_autocmd\tmode\vinsert\ntableÅ\1\1\0\6\0\b\0\0166\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0023\2\3\0006\3\0\0'\5\4\0B\3\2\0029\3\5\0035\5\6\0=\2\a\5B\3\2\0012\0\0ÄK\0\1\0\14generator\1\0\0\nsetup\ael\0\17el.subscribe\18el.extensions\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/express_line.nvim",
    url = "https://github.com/tjdevries/express_line.nvim"
  },
  ["fzf-lua"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/fzf-lua",
    url = "https://github.com/ibhagwan/fzf-lua"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21plugins.gitsigns\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/gitsigns.nvim",
    url = "https://github.com/lewis6991/gitsigns.nvim"
  },
  ["goto-preview"] = {
    config = { "\27LJ\2\n:\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\17goto-preview\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/goto-preview",
    url = "https://github.com/rmagatti/goto-preview"
  },
  ["hop.nvim"] = {
    config = { "\27LJ\2\n1\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\bhop\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/hop.nvim",
    url = "https://github.com/phaazon/hop.nvim"
  },
  ["hover.nvim"] = {
    config = { "\27LJ\2\n3\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\24hover.providers.lsp\frequire°\2\1\0\a\0\17\0!6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0003\3\3\0=\3\5\0025\3\6\0=\3\a\2B\0\2\0016\0\b\0009\0\t\0009\0\n\0'\2\v\0'\3\f\0006\4\0\0'\6\1\0B\4\2\0029\4\1\0045\5\r\0B\0\5\0016\0\b\0009\0\t\0009\0\n\0'\2\v\0'\3\14\0006\4\0\0'\6\1\0B\4\2\0029\4\15\0045\5\16\0B\0\5\1K\0\1\0\1\0\1\tdesc\24hover.nvim (select)\17hover_select\agK\1\0\1\tdesc\15hover.nvim\6K\6n\bset\vkeymap\bvim\17preview_opts\1\0\0\tinit\1\0\2\19preview_window\1\ntitle\2\0\nsetup\nhover\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/hover.nvim",
    url = "https://github.com/lewis6991/hover.nvim"
  },
  ["mini.cursorword"] = {
    config = { "\27LJ\2\n=\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\20mini.cursorword\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/mini.cursorword",
    url = "https://github.com/echasnovski/mini.cursorword"
  },
  ["mini.indentscope"] = {
    config = { "\27LJ\2\nä\1\0\0\a\0\t\0\r6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\a\0005\4\3\0009\5\4\0009\5\5\5B\5\1\2=\5\6\4=\4\b\3B\1\2\1K\0\1\0\tdraw\1\0\0\14animation\tnone\18gen_animation\1\0\1\ndelay\3\0\nsetup\21mini.indentscope\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/mini.indentscope",
    url = "https://github.com/echasnovski/mini.indentscope"
  },
  ["mini.tabline"] = {
    config = { "\27LJ\2\n:\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\17mini.tabline\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/mini.tabline",
    url = "https://github.com/echasnovski/mini.tabline"
  },
  ["neo-tree.nvim"] = {
    config = { "\27LJ\2\n≈\2\0\0\5\0\17\0\0216\0\0\0009\0\1\0009\0\2\0'\2\3\0005\3\4\0B\0\3\0016\0\5\0'\2\6\0B\0\2\0029\0\a\0005\2\v\0005\3\t\0005\4\b\0=\4\n\3=\3\f\0025\3\14\0005\4\r\0=\4\15\3=\3\16\2B\0\2\1K\0\1\0\15filesystem\19filtered_items\1\0\0\1\0\3\16hide_hidden\1\20hide_gitignored\1\18hide_dotfiles\1\30default_component_configs\1\0\0\ticon\1\0\0\1\0\1\17folder_empty\bÓóæ\nsetup\rneo-tree\frequire\1\0\2\ttext\tÔÅö \vtexthl\23DiagnosticSignInfo\23DiagnosticSignInfo\16sign_define\afn\bvim\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/neo-tree.nvim",
    url = "https://github.com/nvim-neo-tree/neo-tree.nvim"
  },
  ["nui.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nui.nvim",
    url = "https://github.com/MunifTanjim/nui.nvim"
  },
  ["null-ls.nvim"] = {
    config = { "\27LJ\2\n/\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\20plugins.null_ls\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/null-ls.nvim",
    url = "https://github.com/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    url = "https://github.com/windwp/nvim-autopairs"
  },
  ["nvim-highlight-colors"] = {
    config = { "\27LJ\2\n.\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\19plugins.colors\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-highlight-colors",
    url = "https://github.com/brenoprata10/nvim-highlight-colors"
  },
  ["nvim-lightbulb"] = {
    config = { "\27LJ\2\n1\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\22plugins.lightbulb\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-lightbulb",
    url = "https://github.com/kosayoda/nvim-lightbulb"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\2\n1\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\22plugins.lspconfig\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-possession"] = {
    config = { "\27LJ\2\n\30\0\0\2\1\1\0\4-\0\0\0009\0\0\0B\0\1\1K\0\1\0\0¿\tlist\29\0\0\2\1\1\0\4-\0\0\0009\0\0\0B\0\1\1K\0\1\0\0¿\bnew \0\0\2\1\1\0\4-\0\0\0009\0\0\0B\0\1\1K\0\1\0\0¿\vupdateŒ\1\1\0\6\0\r\0\0286\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\0016\1\3\0009\1\4\0019\1\5\1'\3\6\0'\4\a\0003\5\b\0B\1\4\0016\1\3\0009\1\4\0019\1\5\1'\3\6\0'\4\t\0003\5\n\0B\1\4\0016\1\3\0009\1\4\0019\1\5\1'\3\6\0'\4\v\0003\5\f\0B\1\4\0012\0\0ÄK\0\1\0\0\14<Space>su\0\14<Space>sn\0\21<Bslash><Bslash>\6n\bset\vkeymap\bvim\nsetup\20nvim-possession\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-possession",
    url = "https://github.com/gennaro-tedesco/nvim-possession"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\nÒ\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\14highlight\1\0\1\venable\2\21ensure_installed\1\0\1\17auto_install\2\1\16\0\0\15typescript\15javascript\bcss\thtml\bsql\rmarkdown\njson5\njsdoc\blua\vvimdoc\bvim\nquery\6c\fcomment\nregex\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/nvim-tree/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  playground = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/playground",
    url = "https://github.com/nvim-treesitter/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["vim-cool"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/vim-cool",
    url = "https://github.com/romainl/vim-cool"
  },
  ["vim-floaterm"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/vim-floaterm",
    url = "https://github.com/voldikss/vim-floaterm"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/vim-surround",
    url = "https://github.com/tpope/vim-surround"
  },
  ["zen-mode.nvim"] = {
    config = { "\27LJ\2\nW\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\vwindow\1\0\0\1\0\1\nwidth\3x\nsetup\rzen-mode\frequire\0" },
    loaded = true,
    path = "/home/nonh/.local/share/nvim/site/pack/packer/start/zen-mode.nvim",
    url = "https://github.com/folke/zen-mode.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: hop.nvim
time([[Config for hop.nvim]], true)
try_loadstring("\27LJ\2\n1\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\bhop\frequire\0", "config", "hop.nvim")
time([[Config for hop.nvim]], false)
-- Config for: nvim-lightbulb
time([[Config for nvim-lightbulb]], true)
try_loadstring("\27LJ\2\n1\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\22plugins.lightbulb\frequire\0", "config", "nvim-lightbulb")
time([[Config for nvim-lightbulb]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
try_loadstring("\27LJ\2\n1\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\22plugins.lspconfig\frequire\0", "config", "nvim-lspconfig")
time([[Config for nvim-lspconfig]], false)
-- Config for: Comment.nvim
time([[Config for Comment.nvim]], true)
try_loadstring("\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\fComment\frequire\0", "config", "Comment.nvim")
time([[Config for Comment.nvim]], false)
-- Config for: hover.nvim
time([[Config for hover.nvim]], true)
try_loadstring("\27LJ\2\n3\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\24hover.providers.lsp\frequire°\2\1\0\a\0\17\0!6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0003\3\3\0=\3\5\0025\3\6\0=\3\a\2B\0\2\0016\0\b\0009\0\t\0009\0\n\0'\2\v\0'\3\f\0006\4\0\0'\6\1\0B\4\2\0029\4\1\0045\5\r\0B\0\5\0016\0\b\0009\0\t\0009\0\n\0'\2\v\0'\3\14\0006\4\0\0'\6\1\0B\4\2\0029\4\15\0045\5\16\0B\0\5\1K\0\1\0\1\0\1\tdesc\24hover.nvim (select)\17hover_select\agK\1\0\1\tdesc\15hover.nvim\6K\6n\bset\vkeymap\bvim\17preview_opts\1\0\0\tinit\1\0\2\19preview_window\1\ntitle\2\0\nsetup\nhover\frequire\0", "config", "hover.nvim")
time([[Config for hover.nvim]], false)
-- Config for: zen-mode.nvim
time([[Config for zen-mode.nvim]], true)
try_loadstring("\27LJ\2\nW\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\vwindow\1\0\0\1\0\1\nwidth\3x\nsetup\rzen-mode\frequire\0", "config", "zen-mode.nvim")
time([[Config for zen-mode.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\nÒ\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\14highlight\1\0\1\venable\2\21ensure_installed\1\0\1\17auto_install\2\1\16\0\0\15typescript\15javascript\bcss\thtml\bsql\rmarkdown\njson5\njsdoc\blua\vvimdoc\bvim\nquery\6c\fcomment\nregex\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: neo-tree.nvim
time([[Config for neo-tree.nvim]], true)
try_loadstring("\27LJ\2\n≈\2\0\0\5\0\17\0\0216\0\0\0009\0\1\0009\0\2\0'\2\3\0005\3\4\0B\0\3\0016\0\5\0'\2\6\0B\0\2\0029\0\a\0005\2\v\0005\3\t\0005\4\b\0=\4\n\3=\3\f\0025\3\14\0005\4\r\0=\4\15\3=\3\16\2B\0\2\1K\0\1\0\15filesystem\19filtered_items\1\0\0\1\0\3\16hide_hidden\1\20hide_gitignored\1\18hide_dotfiles\1\30default_component_configs\1\0\0\ticon\1\0\0\1\0\1\17folder_empty\bÓóæ\nsetup\rneo-tree\frequire\1\0\2\ttext\tÔÅö \vtexthl\23DiagnosticSignInfo\23DiagnosticSignInfo\16sign_define\afn\bvim\0", "config", "neo-tree.nvim")
time([[Config for neo-tree.nvim]], false)
-- Config for: nvim-possession
time([[Config for nvim-possession]], true)
try_loadstring("\27LJ\2\n\30\0\0\2\1\1\0\4-\0\0\0009\0\0\0B\0\1\1K\0\1\0\0¿\tlist\29\0\0\2\1\1\0\4-\0\0\0009\0\0\0B\0\1\1K\0\1\0\0¿\bnew \0\0\2\1\1\0\4-\0\0\0009\0\0\0B\0\1\1K\0\1\0\0¿\vupdateŒ\1\1\0\6\0\r\0\0286\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\0016\1\3\0009\1\4\0019\1\5\1'\3\6\0'\4\a\0003\5\b\0B\1\4\0016\1\3\0009\1\4\0019\1\5\1'\3\6\0'\4\t\0003\5\n\0B\1\4\0016\1\3\0009\1\4\0019\1\5\1'\3\6\0'\4\v\0003\5\f\0B\1\4\0012\0\0ÄK\0\1\0\0\14<Space>su\0\14<Space>sn\0\21<Bslash><Bslash>\6n\bset\vkeymap\bvim\nsetup\20nvim-possession\frequire\0", "config", "nvim-possession")
time([[Config for nvim-possession]], false)
-- Config for: goto-preview
time([[Config for goto-preview]], true)
try_loadstring("\27LJ\2\n:\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\17goto-preview\frequire\0", "config", "goto-preview")
time([[Config for goto-preview]], false)
-- Config for: null-ls.nvim
time([[Config for null-ls.nvim]], true)
try_loadstring("\27LJ\2\n/\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\20plugins.null_ls\frequire\0", "config", "null-ls.nvim")
time([[Config for null-ls.nvim]], false)
-- Config for: mini.tabline
time([[Config for mini.tabline]], true)
try_loadstring("\27LJ\2\n:\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\17mini.tabline\frequire\0", "config", "mini.tabline")
time([[Config for mini.tabline]], false)
-- Config for: mini.indentscope
time([[Config for mini.indentscope]], true)
try_loadstring("\27LJ\2\nä\1\0\0\a\0\t\0\r6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\a\0005\4\3\0009\5\4\0009\5\5\5B\5\1\2=\5\6\4=\4\b\3B\1\2\1K\0\1\0\tdraw\1\0\0\14animation\tnone\18gen_animation\1\0\1\ndelay\3\0\nsetup\21mini.indentscope\frequire\0", "config", "mini.indentscope")
time([[Config for mini.indentscope]], false)
-- Config for: express_line.nvim
time([[Config for express_line.nvim]], true)
try_loadstring("\27LJ\2\nJ\0\2\6\1\2\0\f-\2\0\0009\2\0\2\18\4\0\0\18\5\1\0B\2\3\2\15\0\2\0X\3\4Ä'\3\1\0\18\4\2\0&\3\4\3L\3\2\0K\0\1\0\0\0\n%=Óú•\15git_branchç\1\1\2\v\2\a\0\0194\2\0\0006\3\0\0009\3\1\3\18\5\2\0-\6\0\0009\6\2\6B\3\3\0016\3\0\0009\3\1\3\18\5\2\0-\6\1\0009\6\3\6'\b\4\0'\t\5\0003\n\6\0B\6\4\0A\3\1\0012\0\0ÄL\2\2\0\0¿\1¿\0\rBufEnter\18el_git_branch\16buf_autocmd\tmode\vinsert\ntableÅ\1\1\0\6\0\b\0\0166\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0023\2\3\0006\3\0\0'\5\4\0B\3\2\0029\3\5\0035\5\6\0=\2\a\5B\3\2\0012\0\0ÄK\0\1\0\14generator\1\0\0\nsetup\ael\0\17el.subscribe\18el.extensions\frequire\0", "config", "express_line.nvim")
time([[Config for express_line.nvim]], false)
-- Config for: mini.cursorword
time([[Config for mini.cursorword]], true)
try_loadstring("\27LJ\2\n=\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\20mini.cursorword\frequire\0", "config", "mini.cursorword")
time([[Config for mini.cursorword]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21plugins.gitsigns\frequire\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Config for: nvim-highlight-colors
time([[Config for nvim-highlight-colors]], true)
try_loadstring("\27LJ\2\n.\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\19plugins.colors\frequire\0", "config", "nvim-highlight-colors")
time([[Config for nvim-highlight-colors]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au BufReadPost * ++once lua require("packer.load")({'alternate-toggler'}, { event = "BufReadPost *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")

_G._packer.inside_compile = false
if _G._packer.needs_bufread == true then
  vim.cmd("doautocmd BufRead")
end
_G._packer.needs_bufread = false

if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
