local namespace = vim.api.nvim_create_namespace('iqf')

-- function _G.quickfixtextfunc(info)
--     local items, list
--     if info.quickfix > 0 then
--         list = vim.fn.getqflist({ id = info.id, items = 0, qfbufnr = 1 })
--     else
--         list = vim.fn.getloclist(info.winid, { id = info.id, items = 0, qfbufnr = 1 })
--     end
--     items = list.items
--
--     local max_width = vim.api.nvim_get_option('columns')
--     local function get_item_fname(item)
--         local fname = item.bufnr > 0 and vim.fn.bufname(item.bufnr) or ''
--         local display = nil
--         local style = nil
--
--         if fname == '' then
--             display = '[No Name]'
--         else
--             fname = vim.fn.fnamemodify(fname, ':p:~:.')
--             display, style = require('telescope.utils').transform_path({ path_display = { 'filename_first' } }, fname)
--         end
--
--         -- local len = vim.fn.strchars(fname)
--         -- if len > 20 then
--         --   fname = "…" .. vim.fn.strpart(fname, len - 20, len, true)
--         -- end
--
--         return fname, display, style
--     end
--
--     local fname_limit = 1
--     local lnum_limit = 1
--     local col_limit = 1
--
--     for _, item in ipairs(items) do
--         local fname, display, style = get_item_fname(item)
--         item._file = { filename = fname, display = display, style = style }
--         local lnum = '' .. item.lnum
--         local col = '' .. item.col
--
--         if #display > fname_limit then
--             fname_limit = #display
--         end
--         if #lnum > lnum_limit then
--             lnum_limit = #lnum
--         end
--         if #col > col_limit then
--             col_limit = #col
--         end
--     end
--
--     local function type_to_value(type)
--         if type == 'E' then
--             return 'error'
--         elseif type == 'W' then
--             return 'warning'
--         elseif type == 'N' then
--             return 'note'
--         elseif type == 'I' then
--             return 'info'
--         end
--         return type
--     end
--
--     local highlights = {}
--     local counter = 0
--     local function format_item(item)
--         if item.valid == 1 then
--             local fname = item._file.filename
--             local display = item._file.display
--             local extension = vim.fn.fnamemodify(fname, ':t:e')
--             local icon, icon_hl = require('nvim-web-devicons').get_icon(fname, extension)
--             if not icon or icon == 'nil' then
--                 icon = ''
--             else
--                 table.insert(highlights, { line = counter, group = icon_hl })
--             end
--             local highlight = item._file.style[1]
--             table.insert(
--                 highlights,
--                 { line = counter, col = highlight[1][1] + 5, end_col = highlight[1][2] + 5, group = highlight[2] }
--             )
--             counter = counter + 1
--
--             local lnum = '' .. item.lnum
--             local col = '' .. item.col
--
--             return ('%s  %s | %s col %s%s | %s'):format(
--                 icon,
--                 display .. string.rep(' ', fname_limit - #fname),
--                 string.rep(' ', lnum_limit - #lnum) .. lnum,
--                 col .. string.rep(' ', col_limit - #col),
--                 item.type == '' and '' or ' ' .. type_to_value(item.type),
--                 item.text:gsub('^%s+', ''):gsub('\n', ' ')
--             )
--         else
--             return item.text
--         end
--     end
--     vim.schedule(function()
--         local id = list.qfbufnr
--         for _, hl in ipairs(highlights) do
--             local col = hl.col or 0
--             local end_col = hl.end_col or 2
--             vim.highlight.range(id, namespace, hl.group, { hl.line, col }, { hl.line, end_col })
--         end
--     end)
--     return vim.tbl_map(format_item, vim.list_slice(items, info.start_idx, info.end_idx))
-- end

local options = {
    ai = true,
    autoindent = true,
    autowrite = true,
    background = 'light',
    backspace = 'indent,eol,start',
    backup = false, -- creates a backup file
    breakindent = true,
    clipboard = 'unnamedplus', -- allows neovim to access the system clipboard
    cmdheight = 1, -- more space in the neovim command line for displaying messages
    completeopt = 'menu,menuone,noselect', -- mostly just for cmp
    conceallevel = 0, -- so that `` is visible in markdown files
    confirm = true, -- Confirm to save changes before exiting modified buffer
    cursorline = false, -- highlight the current line
    expandtab = true, -- convert tabs to spaces
    fileencoding = 'utf-8', -- the encoding written to a file
    formatoptions = 'jcroqlnt', -- tcqj
    grepformat = '%f:%l:%c:%m',
    grepprg = 'rg --vimgrep',
    hlsearch = true, -- highlight all matches on previous search pattern
    ignorecase = true, -- ignore case in search patterns
    inccommand = 'split', -- preview incremental substitute
    incsearch = true,
    laststatus = 3,
    list = false,
    linebreak = true,
    -- listchars = { trail = '', tab = '', nbsp = '_', extends = '>', precedes = '<' }, -- highlight
    -- listchars = { tab = '» ', trail = '·', nbsp = '␣' },
    mouse = 'a', -- allow the mouse to be used in neovim
    number = true, -- set numbered lines
    -- numberwidth = 4,        -- set number column width to 2 {default 4}
    pumblend = 0, -- Popup blend
    pumheight = 10, -- pop up menu height
    quickfixtextfunc = [[{info -> v:lua.quickfixtextfunc(info)}]],
    relativenumber = false, -- set relative numbered lines
    scrolloff = 8, -- is one of my fav
    sessionoptions = 'blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal',
    shiftround = true, -- Round indent
    shiftwidth = 4, -- the number of spaces inserted for each indentation
    showcmd = false,
    showmode = true, -- we don't need to see things like -- INSERT -- anymore
    showtabline = 0, -- always show tabs
    si = true,
    sidescrolloff = 8,
    signcolumn = 'yes', -- always show the sign column, otherwise it would shift the text each time
    smartcase = true, -- smart case
    smartindent = true, -- make indenting smarter again
    smarttab = true,
    softtabstop = 4,
    splitbelow = true, -- force all horizontal splits to go below current window
    splitright = true, -- force all vertical splits to go to the right of current window
    swapfile = false, -- creates a swapfile
    tabstop = 4, -- insert 2 spaces for a tab
    termguicolors = true, -- set term gui colors (most terminals support this)
    timeoutlen = 300, -- time to wait for a mapped sequence to complete (in milliseconds)
    title = true,
    undofile = true, -- enable persistent undo
    undodir = vim.fn.expand('~/.config/nvim/undo'),
    undolevels = 10000,
    updatetime = 50, -- faster completion (4000ms default)
    wildmenu = true, -- wildmenu
    wildmode = 'longest:full,full', -- Command-line completion mode
    winminwidth = 5, -- Minimum window width
    wrap = false, -- display lines as one long line
    writebackup = false, -- do not edit backups
}

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

---------------------------------------------------------------------------------------------------
-- Setup python path
---------------------------------------------------------------------------------------------------
local possible_python_paths = {
    -- Extend the list for possible python path. Will use the 1st possible one
    -- os.getenv('HOME') .. '/qa/automaps_jm_landing/.venv/bin/python3', -- Python3's venv (dev)
    -- os.getenv("HOME") .. "/opt/anaconda3/envs/dev/bin/python",          -- MacOS's conda (dev)
    -- os.getenv("HOME") .. "/anaconda3/envs/dev/bin/python",              -- Linux's conda (dev)
    -- os.getenv('HOME') .. '/.conda/envs/dev/bin/python',                 -- Linux's alternative conda (dev)
    -- os.getenv("HOME") .. "/.pyenv/shims/python",                        -- pyenv's default path
    '/usr/bin/python3', -- System default python3
    '/usr/bin/python', -- System default python
}
for _, python_path in pairs(possible_python_paths) do
    if io.open(python_path, 'r') ~= nil then
        vim.g.python3_host_prog = python_path
        break
    end
end

-- vim.g.python3_host_prog = '~/venvs/neovim/bin/python'

---------------------------------------------------------------------------------------------------
-- Deactivate unused providers
---------------------------------------------------------------------------------------------------
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0

-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0

vim.opt.shortmess:append({ W = true, I = true, c = true })

if vim.fn.has('nvim-0.9.0') == 1 then
    vim.opt.splitkeep = 'screen'
    vim.opt.shortmess:append({ C = true })
end

for k, v in pairs(options) do
    vim.opt[k] = v
end

vim.opt.fillchars = { eob = ' ', fold = '.', foldopen = '', foldclose = '', foldsep = ' ' }

--- Statusline
-- cf the default statusline: %<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

-- format markers:
--   %< truncation point
--   %n buffer number
--   %f relative path to file
--   %m modified flag [+] (modified), [-] (unmodifiable) or nothing
--   %r readonly flag [RO]
--   %y filetype [ruby]
--   %= split point for left and right justification
--   %-35. width specification
--   %l current line number
--   %L number of lines in buffer
--   %c current column number
--   %V current virtual column number (-n), if different from %c
--   %P percentage through buffer
--   %) end of width specification

-- vim.opt.statusline = '%<%f %h%m%r%=%-14.(%l,%c%V%) %P'
-- vim.opt.statusline = '%<%f %h%m%r'
-- vim.opt.statusline = "%#WinSeparator#%{%v:lua.string.rep('—', v:lua.vim.fn.winwidth(0))%}"
-- require('core.echoline')
