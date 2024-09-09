-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- Font
config.font = wezterm.font 'JetBrains Mono'

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

-- Linux
if wezterm.target_triple:find("linux") then
end

-- macOS
if wezterm.target_triple == 'x86_64-apple-darwin' or
   wezterm.target_triple == 'aarch64-apple-darwin' then
   config.font_size = 15
   config.default_prog = { '/bin/zsh','-l' }
   config.launch_menu = {
      {
	 args = { 'top' },
      },
      {
	 label = 'Google chrome',
	 args = {
	    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
	 },
      },
      {
	 label = 'Perplexity',
	 args = {
	    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',	    
	    '--app=https://www.perplexity.ai/',
	 },
      },
      {
	 label = 'GitHub',
	 args = {
	    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',	    
	    '--app=https://github.com/',
	 }
      }
   }
end

-- Windows
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
   config.font_size = 14
   config.default_prog = { 'pwsh.exe' }
   config.launch_menu = {
      {
	 args = { 'top' },
      },
      {
	 label = 'WSL',
	 args = {
	    'wsl.exe',
	    '--distribution',
	    'ubuntu',
	    '--cd',
	    '~'
	 },
      },
      {
	 label = 'WSL (C)',
	 args = {
	    'wsl.exe',
	    '--distribution',
	    'ubuntu',
	    '--cd',
	    '/mnt/c/Users'
	 },
      },
      {
	 label = 'Windows Explorer',
	 args = {
	    'explorer.exe',
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
      {
	 label = 'Visual Studio 2022',
	 args = {
	    'C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/devenv.exe',
	 },
      },
      {
	 label = 'Google chrome',
	 args = {
	    'C:/Program Files/Google/Chrome/Application/chrome.exe',
	 },
      },
      {
	 label = 'Perplexity',
	 args = {
	    'C:/Program Files/Google/Chrome/Application/chrome.exe',
	    '--app=https://www.perplexity.ai/',
	 },
      },
      {
	 label = 'GitHub',
	 args = {
	    'C:/Program Files/Google/Chrome/Application/chrome.exe',
	    '--app=https://github.com/',
	 },
      },
   }
end

return config
