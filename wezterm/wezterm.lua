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

-- config.color_schemes = {
-- 	["Alabaster Dark"] = {
-- 		background = "#0e1415",
-- 		foreground = "#cecece",
-- 		cursor_bg = "#cd974b",
-- 		cursor_border = "#cd974b",
-- 		cursor_fg = "#0e1415",
-- 		selection_bg = "#293334",
-- 		selection_fg = "#cecece",
-- 		ansi = {
-- 			"#000000",
-- 			"#d2322d",
-- 			"#6abf40",
-- 			"#cd974b",
-- 			"#217EBC",
-- 			"#9B3596",
-- 			"#178F79",
-- 			"#cecece",
-- 		},
-- 		brights = {
-- 			"#333333",
-- 			"#c33c33",
-- 			"#95cb82",
-- 			"#dfdf8e",
-- 			"#71aed7",
-- 			"#cc8bc9",
-- 			"#47BEA9",
-- 			"#ffffff",
-- 		},
-- 	},
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
-- config.window_padding = {
-- 	top = 0,
-- 	bottom = 0,
-- 	left = 0,
-- 	right = 0,
-- }
config.font_size = 16
-- config.window_decorations = "RESIZE"
config.default_cursor_style = "SteadyBlock"
config.launch_menu = launch_menu
config.show_new_tab_button_in_tab_bar = false
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
		key = "t",
		mods = "SHIFT|CTRL",
		action = act.SpawnTab("CurrentPaneDomain"),
	},
	{
		key = "3", -- C-x 3 emacs
		mods = "ALT",
		action = wezterm.action.SplitPane({
			direction = "Right",
			size = { Percent = 40 },
		}),
	},
	{
		key = "2",
		mods = "ALT",
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
	brightness = 0.65,
}

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
	config.color_scheme = "nightfox"
	config.window_background_image = "C:/Users/Максим/cas.jpg"
	config.default_domain = "WSL:Ubuntu"
	config.window_background_image_hsb = {
		-- Darken the background image by reducing it to 1/3rd
		brightness = 0.025,

		-- You can adjust the hue by scaling its value.
		-- a multiplier of 1.0 leaves the value unchanged.
		hue = 1.0,

		-- You can adjust the saturation also.
		saturation = 1.0,
	}
end

-- if wezterm.target_triple == "x86_64-unknown-linux-gnu" then
-- end

return config
