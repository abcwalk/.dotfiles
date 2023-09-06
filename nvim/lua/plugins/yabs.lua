return {
  'shadowofseaice/yabs.nvim',
  config = function()
    require 'yabs'.setup {
      position = { 'C' }, -- {'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N', 'C'}
      settings = {
        { 'name' },
        { 'icon', 'bufnr', 'bufname', 'lnum', 'line' },
        { 'path', 'name',  'bufid' },
      },
      keymap = {
        close     = "D",    -- Close buffer. Default D
        jump      = "<CR>", -- Jump to buffer. Default <cr>
        h_split   = "s",    -- Horizontally split buffer. Default s
        v_split   = "v",    -- Vertically split buffer. Default v
        pinning   = "p",    -- Open buffer preview. Default p
        cycset    = ">",    -- Cycle through settings, Default ]
        rcycset   = "<",    -- Reverse cycle through settings, Default [
        cycpos    = "}",    -- Cycle through settings, Default >
        rcycpos   = "{",    -- Reverse cycle through panel placement, Default <
        cycname   = "]",    -- Cycle through file name type, Default }
        rcycname  = "[",    -- Reverse cycle through file name type, Default {
        cychdr    = "T",    -- Cycle through group headerlazyions, Default H
        sortpath  = "P",    -- Sort by file path. Default P
        sortext   = "e",    -- Sort by file extension (type), Default t
        sortbuf   = "x",    -- Sort clear = sort by buffer #, default c
        sortbase  = "b",    -- Sort by file base name #, default f
        sortfull  = "f",    -- Sort by full file name #, default F
        sortsetup = "i",    -- Sort by file name initial #, default i
      },
      rnu = false,
      border = "rounded",
      -- symbols = {
      -- at most two of these icons can be shown for a given buffer
      -- current   = "C", -- default 
      -- split     = "S", -- default 
      -- alternate = "A", -- default 
      -- unloaded  = "H", -- default
      -- locked    = "L", -- default 
      -- ro        = "R", -- default 
      -- edited    = "E", -- default 
      -- terminal  = "T", -- default 
      -- more      = ">", -- default "", when the panel size is too small for file name
      -- grphead   = "-", -- default " ",
      -- grptop    = "+", -- default "╭",
      -- grpmid    = "|", -- default "│",
      -- grpbot    = "+", -- default "╰",
      -- pinned    = "P", -- default "",
      -- filedef   = "D", -- Filetype icon if not present in nvim-web-devicons. Default 
      -- }
    }
  end,
}
