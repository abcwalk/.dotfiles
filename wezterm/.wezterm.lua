local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action

local config = {}
local mouse_bindings = {}
local launch_menu = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

wezterm.on("gui-startup", function()
    local tab, pane, window = mux.spawn_window({})
    window:gui_window():maximize()
end)

config.color_scheme = "rose-pine"
config.colors = {
    background = "#0E0A00",
    foreground = "#d8dacb",
    cursor_bg = "#f3eeea",
    cursor_fg = "#090a18",
}
config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font_size = 18
config.default_cursor_style = "SteadyBlock"
config.launch_menu = launch_menu
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.adjust_window_size_when_changing_font_size = false
config.disable_default_key_bindings = true
config.keys = {
    { key = "Insert", mods = "SHIFT", action = act.PasteFrom("Clipboard") },
    {
        key = "t",
        mods = "CTRL|SHIFT",
        action = act.ShowTabNavigator,
    },
    {
        key = "n",
        mods = "CTRL|SHIFT",
        action = act.SpawnTab("CurrentPaneDomain"),
    },
    {
        key = "h",
        mods = "CTRL|SHIFT",
        action = wezterm.action.SplitPane({
            direction = "Right",
            size = { Percent = 40 },
        }),
    },
    { key = "8",      mods = "CTRL",  action = act.PaneSelect },
    { key = ",",      mods = "ALT",   action = act.ActivateTabRelative(-1) },
    { key = ".",      mods = "ALT",   action = act.ActivateTabRelative(1) },
}
config.mouse_bindings = mouse_bindings
config.default_domain = "WSL:Ubuntu"

return config
