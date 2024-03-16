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

wezterm.on("update-right-status", function(window, pane)
	local cells = {}

	local date = wezterm.strftime("%a %-d %b - %H:%M:%S")
	table.insert(cells, date)

	for _, b in ipairs(wezterm.battery_info()) do
		table.insert(cells, string.format("%.0f%%", b.state_of_charge * 100))
	end

	local statusline_colors = {
		"#1c1c1c",
	}

	local text_fg = "#7799bb"

	local elements = {}
	local num_cells = 0

	local function push(text, is_last)
		local cell_no = num_cells + 1
		table.insert(elements, { Foreground = { Color = text_fg } })
		-- table.insert(elements, { Background = { Color = statusline_colors[cell_no] } })
		table.insert(elements, { Text = text })
		if not is_last then
			table.insert(elements, { Foreground = { Color = statusline_colors[cell_no + 1] } })
		end
		num_cells = num_cells + 1
	end

	while #cells > 0 do
		local cell = table.remove(cells, 1)
		push(cell, #cells == 0)
	end

	window:set_right_status(wezterm.format(elements))
end)

config.color_scheme = "Black Metal (base16)"
-- config.color_schemes = {
--     ["rose-pine"] = {
--         background = "#1b160a",
--         foreground = "#d8dacb",
--         cursor_bg = "#f3eeea",
--         cursor_fg = "#090a18",
--     },
-- }
-- config.window_background_opacity = 0.7
config.font = wezterm.font("JetBrainsMono Nerd Font")
config.window_frame = {
	font = wezterm.font({ family = "JetBrainsMono Nerd Font" }),
	font_size = 14.0,
	-- active_titlebar_bg = colors.dark_palette.bg0,
	-- active_titlebar_fg = colors.dark_palette.fg0,
	-- inactive_titlebar_bg = colors.dark_palette.bg1,
	-- inactive_titlebar_fg = colors.dark_palette.fg1,
}
config.tab_bar_at_bottom = true
config.window_padding = {
	top = 15,
	bottom = 15,
}
config.font_size = 18
-- config.window_decorations = "TITLE"
config.default_cursor_style = "SteadyBlock"
config.launch_menu = launch_menu
config.use_fancy_tab_bar = false
-- config.hide_tab_bar_if_only_one_tab = true
config.colors = {
	tab_bar = {
		background = "#121212",
		-- new_tab = { bg_color = "#121212", fg_color = "#FCE8C3", intensity = "Bold" },
		-- new_tab_hover = { bg_color = "#121212", fg_color = "#FBB829", intensity = "Bold" },
		-- active_tab = { bg_color = "#121212", fg_color = "#FCE8C3" },
	},
	-- saturate(0.1),
}
config.window_close_confirmation = "NeverPrompt"
config.enable_scroll_bar = false
config.audible_bell = "Disabled"
config.adjust_window_size_when_changing_font_size = false
config.disable_default_key_bindings = true
config.keys = {
	{ key = "Insert", mods = "SHIFT", action = act.PasteFrom("Clipboard") },
	{
		key = "N",
		mods = "SHIFT|CTRL",
		action = act.SpawnTab("CurrentPaneDomain"),
	},
	{
		key = "|",
		mods = "SHIFT",
		action = wezterm.action.SplitPane({
			direction = "Right",
			size = { Percent = 40 },
		}),
	},
	{
		key = "_",
		mods = "SHIFT",
		action = wezterm.action.SplitPane({
			direction = "Down",
			size = { Percent = 40 },
		}),
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
	-- { key = "w",          mods = "ALT",        action = act.CloseCurrentTab({ confirm = false }) },
	{
		key = "Enter",
		mods = "ALT",
		action = wezterm.action.ToggleFullScreen,
	},
}
config.mouse_bindings = mouse_bindings
config.inactive_pane_hsb = {
	saturation = 0.9,
	brightness = 0.5,
}
config.window_background_image = "C:/Users/Максим/castle.jpg"
config.window_background_image_hsb = {
	-- Darken the background image by reducing it to 1/3rd
	brightness = 0.04,

	-- You can adjust the hue by scaling its value.
	-- a multiplier of 1.0 leaves the value unchanged.
	hue = 5.0,

	-- You can adjust the saturation also.
	saturation = 1.0,
}
config.default_domain = "WSL:Ubuntu"

return config
