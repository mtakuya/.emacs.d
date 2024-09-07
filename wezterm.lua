-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- Font
config.font = wezterm.font 'JetBrains Mono'
config.font_size = 15.0

-- Disable ligatures
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- Color scheme
config.color_scheme = 'Andromeda'

-- Color
config.colors = {
  cursor_bg = '#FF69B4',
  cursor_fg = '#000000',
  cursor_border = '#FF69B4',
  tab_bar = {
    background = "#0F0F0F",
    active_tab = {
      bg_color = "#262A33",
      fg_color = "#C0C0C0",
    },
    inactive_tab = {
      bg_color = "#333333",
      fg_color = "#C0C0C0",
    },
  },
}

-- Cursor style
config.default_cursor_style = 'SteadyBlock'

-- Window frame
config.window_frame = {
  font = require('wezterm').font 'JetBrains Mono',
  font_size = 15.0,
}

-- Hiding the tab bar（if there is only one tab)
config.hide_tab_bar_if_only_one_tab = true

-- Don't make a sound
config.audible_bell = "Disabled"

-- Scroll lines
config.scrollback_lines = 10000

-- Window padding
config.window_padding = {
 left = 5,
 right = 5,
 top = 5,
 bottom = 0,
}

-- Scroll bar
config.enable_scroll_bar = true

-- Hyperlinks
config.hyperlink_rules = wezterm.default_hyperlink_rules()

-- Settings for adjusting window size when changing font size
config.adjust_window_size_when_changing_font_size = false

-- Windows
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
  config.default_prog = { 'pwsh.exe' }
  config.launch_menu = {
    {
      args = { 'top' },
    },
    {
      label = 'WSL2',
      args = {
        'wsl.exe',
        '--distribution',
        'ubuntu',
        '--cd',
        '~'
      },
    },
    {
      label = 'x64 Native Tools VS 2022',
      args = {
        'cmd.exe',
        '/k',
        'C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Auxiliary/Build/vcvars64.bat'
      },
    },
  }
end

return config