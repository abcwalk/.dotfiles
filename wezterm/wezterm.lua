local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action

local DESKTOP_ENV = (os.getenv("XDG_CURRENT_DESKTOP")):lower() or ""
local THEME_FILE_PATH = os.getenv("HOME") .. "/.theme"
local THEME_LIGHT = "Alabaster Light"
local THEME_DARK = "Alabaster Dark"

local config = {}
if wezterm.config_builder then
	config = wezterm.config_builder()
end

local tab_bar_colors = {
	["Alabaster Light"] = {
		background = "#c9c9c9",
		active_tab = { fg_color = "#000000", bg_color = "#aaaaaa" },
		inactive_tab = { fg_color = "#757575", bg_color = "#c9c9c9" },
		inactive_tab_hover = { fg_color = "#7d7d7d", bg_color = "#b3b3b3" },
	},
	["Alabaster Dark"] = {
		background = "#0e1415",
		active_tab = { fg_color = "#95cb82", bg_color = "#162022" },
		inactive_tab = { fg_color = "#7d7d7d", bg_color = "#0e1415" },
		inactive_tab_hover = { fg_color = "#7d7d7d", bg_color = "#202829" },
	},
	["cobalt2"] = {
		background = "#002240",
		active_tab = { fg_color = "#f0cc09", bg_color = "#001b33" },
		inactive_tab = { fg_color = "#cccccc", bg_color = "#002240" },
		inactive_tab_hover = { fg_color = "#7d7d7d", bg_color = "#1a3954" },
	},
	["Ef-Eagle"] = {
		background = "#e4dbc0",
		active_tab = { fg_color = "#882000", bg_color = "#ecdfba" },
		inactive_tab = { fg_color = "#231a1f", bg_color = "#e4dbc0" },
		inactive_tab_hover = { fg_color = "#231a1f", bg_color = "#ddc5af" },
	},
}

local function ensure_theme_file_exists()
	local f = io.open(THEME_FILE_PATH, "r")
	if f then
		f:close()
		return
	end

	f = io.open(THEME_FILE_PATH, "w")
	if f then
		f:write("light\n")
		f:close()
	end
end

local function read_mode_file()
	ensure_theme_file_exists()

	local f = io.open(THEME_FILE_PATH, "r")
	if not f then
		return "light"
	end
	local content = f:read("*line"):lower()
	f:close()
	return content
end

local function set_mode_file(theme)
	local f = io.open(THEME_FILE_PATH, "w")
	if f then
		f:write(theme)
		f:close()
	end
end

local function get_mode()
	local theme_from_file = read_mode_file()
	if theme_from_file then
		return theme_from_file
	end

	return "dark"
end

local function get_theme_config()
	local mode = get_mode()
	local theme_name = mode == "light" and THEME_LIGHT or THEME_DARK

	local bar_colors = tab_bar_colors[theme_name] or {}

	return {
		color_scheme = theme_name,
		colors = {
			tab_bar = {
				background = bar_colors.background,
				active_tab = bar_colors.active_tab,
				inactive_tab = bar_colors.inactive_tab,
				inactive_tab_hover = bar_colors.inactive_tab_hover,
			},
		},
	}
end

local function set_wezterm_theme()
	local theme_config = get_theme_config()
	config.color_scheme = theme_config.color_scheme
	config.colors = theme_config.colors
end

local function set_gnome_gtk_mode(mode)
	local commands = {}

	local gtk_theme = mode == "light" and "Yaru" or "Yaru-dark"
	commands = {
		{ "gsettings", "set", "org.gnome.desktop.interface", "color-scheme", "prefer-" .. mode },
		{ "gsettings", "set", "org.gnome.desktop.interface", "gtk-theme", gtk_theme },
	}

	for _, cmd in ipairs(commands) do
		local success, _, stderr = wezterm.run_child_process(cmd)
		if not success then
			wezterm.log_error("Failed to run gsettings command:", stderr, cmd)
		end
	end
end

local function toggle_theme()
	local current = get_mode()
	local new_theme = current == "light" and "dark" or "light"

	set_mode_file(new_theme)

	if DESKTOP_ENV:find("gnome") then
		set_gnome_gtk_mode(new_theme)
	end

	set_wezterm_theme()
end

wezterm.on("gui-startup", function()
	local _, _, window = mux.spawn_window({})
	window:gui_window():maximize()
	set_wezterm_theme()
end)

config.keys = {
	{ key = "Insert", mods = "SHIFT", action = act.PasteFrom("Clipboard") },
	{
		key = "t",
		mods = "SHIFT|CTRL",
		action = act.SpawnTab("CurrentPaneDomain"),
	},
	{
		key = "3",
		mods = "ALT",
		action = wezterm.action.SplitPane({ direction = "Right", size = { Percent = 40 } }),
	},
	{
		key = "2",
		mods = "ALT",
		action = wezterm.action.SplitPane({ direction = "Down", size = { Percent = 40 } }),
	},
	{ key = "8", mods = "CTRL", action = act.PaneSelect },
	{ key = ",", mods = "ALT", action = act.ActivateTabRelative(-1) },
	{ key = ".", mods = "ALT", action = act.ActivateTabRelative(1) },
	{ key = "+", mods = "SHIFT|CTRL", action = act.IncreaseFontSize },
	{ key = "-", mods = "CTRL", action = act.DecreaseFontSize },
	{ key = "0", mods = "CTRL", action = act.ResetFontSize },
	{ key = "RightArrow", mods = "SHIFT", action = act.ActivatePaneDirection("Right") },
	{ key = "LeftArrow", mods = "SHIFT", action = act.ActivatePaneDirection("Left") },
	{ key = "UpArrow", mods = "SHIFT", action = act.ActivatePaneDirection("Up") },
	{ key = "DownArrow", mods = "SHIFT", action = act.ActivatePaneDirection("Down") },
	{
		key = "Enter",
		mods = "ALT",
		action = wezterm.action.ToggleFullScreen,
	},
	{
		key = "q",
		mods = "ALT",
		action = wezterm.action.CloseCurrentPane({ confirm = true }),
	},
	{
		key = "8",
		mods = "ALT",
		action = wezterm.action_callback(function(_, _)
			toggle_theme()
		end),
	},
}

config.default_prog = { "zsh" }
config.font = wezterm.font("JetBrainsMono Nerd Font", { weight = "Regular" })
config.font_size = 14
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.show_new_tab_button_in_tab_bar = false
config.window_padding = { top = 30, bottom = 30, left = 30, right = 30 }
config.window_decorations = "NONE"
config.default_cursor_style = "SteadyBlock"
config.window_close_confirmation = "NeverPrompt"
config.enable_scroll_bar = false
config.audible_bell = "Disabled"
config.adjust_window_size_when_changing_font_size = false
config.disable_default_key_bindings = true
config.mouse_wheel_scrolls_tabs = false
config.inactive_pane_hsb = { saturation = 0.9, brightness = 0.9 }
config.automatically_reload_config = true
config.warn_about_missing_glyphs = false
config.harfbuzz_features = { "ss07", "calt", "liga=0" }
-- config.window_background_opacity = 0.9
config.max_fps = 120
config.window_frame = {
	font = wezterm.font("JetBrainsMono Nerd Font"),
	font_size = 14.0,
	active_titlebar_bg = "#001e27",
	active_titlebar_fg = "#708284",
	inactive_titlebar_bg = "#001e27",
	inactive_titlebar_fg = "#708284",
}

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
	config.default_domain = "WSL:Ubuntu"
end

set_wezterm_theme()

return config
