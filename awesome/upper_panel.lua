-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local cairo = require("lgi").cairo

-- Theme handling library
local beautiful = require("beautiful")
-- power management
local pm = require("battery-control")
-- volume library
local volume_control = require("volume-control")

-- Create a textclock widget
mytextclock = wibox.widget.textclock()
-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- define your volume control, using default settings:
volumecfg = volume_control({tooltip=true})


-- @table names
local button_names = {
    LEFT        = 1,-- The left mouse button.
    MIDDLE      = 2,-- The scrollwheel button.
    RIGHT       = 3,-- The context menu button.
    SCROLL_UP   = 4,-- A scroll up increment.
    SCROLL_DOWN = 5,-- A scroll down increment.
}
	--- taglist and tasklist buttons {{{
		local taglist_buttons = gears.table.join(
							awful.button({ }, button_names.LEFT, function(t) t:view_only() end),
							awful.button({ modkey }, button_names.LEFT, function(t)
													  if client.focus then
														  client.focus:move_to_tag(t)
													  end
												  end),
							awful.button({ }, button_names.RIGHT, awful.tag.viewtoggle),
							awful.button({ modkey }, button_names.RIGHT, function(t)
													  if client.focus then
														  client.focus:toggle_tag(t)
													  end
												  end),
							awful.button({ }, button_names.SCROLL_UP, function(t) awful.tag.viewnext(t.screen) end),
							awful.button({ }, button_names.SCROLL_DOWN, function(t) awful.tag.viewprev(t.screen) end)
						)

		local tasklist_buttons = gears.table.join(
							 awful.button({ }, button_names.LEFT, function (c)
													  if c == client.focus then
														  c.minimized = true
													  else
														  c:emit_signal(
															  "request::activate",
															  "tasklist",
															  {raise = true}
														  )
													  end
												  end),
							 awful.button({ }, button_names.MIDDLE, function (c)
														  c.minimized = true
												  end),
							 awful.button({ }, button_names.RIGHT, function()
													  awful.menu.client_list({ theme = { width = 250 } })
												  end),
							 awful.button({ }, button_names.SCROLL_UP, function ()
													  awful.client.focus.byidx(1)
												  end),
							 awful.button({ }, button_names.SCROLL_DOWN, function ()
													  awful.client.focus.byidx(-1)
												  end))
		  --- }}}

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

rrect = function(radius)
    return function(cr, width, height)
        gears.shape.rounded_rect(cr, width, height, radius)
    end
end

my_parallelogram = function(slope)
	return function(cr, width, height)
		gears.shape.parallelogram(cr, width, height, width - (height / slope))
	end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,

		style   = {
			bg_focus = '#d3d3d3',
			fg_focus = '#003040',
			fg_empty = '#e0ffff',
			bg_empty = '#1a1823',
			fg_occupied = '#98da95',
			bg_occupied = '#1a1823',
			shape_border_width = 0,
			shape				 = rrect(15),
			shape_border_width_focus = 1,
			shape_border_color_focus = "#204058",
		},
        layout = {
        	layout = wibox.layout.flex.horizontal,
        	spacing=10,
        },

		widget_template = {
			{
				{
					id     = 'text_role',
					widget = wibox.widget.textbox,
				},
				valign	= 'center',
				halign	= 'center',
				layout = wibox.container.place,
			},
			id     = 'background_role',
			widget = wibox.container.background,
			forced_width = 30,
			forced_height = 10,

            -- Add support for hover colors and an index label
            create_callback = function(self, tag, index, objects)
                self:get_children_by_id("text_role")[1].markup = '<b> '..index..' </b>'
                self:connect_signal('mouse::enter', function()
                    if not self.backup_bg then
                        self.backup_bg     = self.bg
                        self.backup_fg     = self.fg
                    end
                    self.had_focus = tag.selected
                    self.bg = '#40cfd8'
                    self.fg = '#111111'
                end)
                self:connect_signal('mouse::leave', function()
                    if self.backup_bg then
                    	-- need to take care of the special case in
                    	-- which a tag got selected (and had its color
                    	-- changed by the 'awesome' code
                    	if not tag.selected or self.had_focus then
							self.bg = self.backup_bg
							self.fg = self.backup_fg
						end
                    end
					self.backup_bg = nil
					self.backup_fg = nil
                end)
            end,
            update_callback = function(self, c3, index, objects) --luacheck: no unused args
                self:get_children_by_id("text_role")[1].markup = '<b> '..index..' </b>'
            end,
		},
        buttons = taglist_buttons
    }

	s.mytasklist = awful.widget.tasklist {
		screen   = s,
		filter   = awful.widget.tasklist.filter.currenttags,
		buttons  = tasklist_buttons,
		style    = {
			shape_border_width	= 1,
			shape_border_color	= '#777777',
			bg_normal			= '#000000',
			bg_focus			= '#000000',
			bg_minimize			= '#1a1823',
			shape  = gears.shape.rounded_bar,
		},
		layout   = {
			spacing = 10,
			spacing_widget = {
				{
					forced_width = 5,
					shape        = gears.shape.circle,
					widget       = wibox.widget.separator
				},
				valign = 'center',
				halign = 'center',
				widget = wibox.container.place,
			},
			layout  = wibox.layout.flex.horizontal
		},
		-- Notice that there is *NO* wibox.wibox prefix, it is a template,
		-- not a widget instance.
		widget_template = {
			{
				{
					{
						{
							id     = 'icon_role',
							widget = wibox.widget.imagebox,
						},
						margins = 2,
						widget  = wibox.container.margin,
					},
					{
						id     = 'text_role',
						widget = wibox.widget.textbox,
					},
					layout = wibox.layout.fixed.horizontal,
				},
				left  = 10,
				right = 10,
				widget = wibox.container.margin
			},
			id     = 'background_role',
			widget = wibox.container.background,
		},
	}
	-- TODO: remove later from here
    -- Create the wibox
    s.mywibox = awful.wibar({
    	position = "top",
    	screen = s,
		height = 34,
        bg = '#000000BB',
    })

	local space_sep = wibox.widget.textbox("   ")
    -- Add widgets to the wibox
    s.mywibox:setup {
        widget	= wibox.container.margin,
		bottom	= 8,
		top = 2,
		{
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
                           layout = wibox.layout.fixed.horizontal,
                           --img_widget,
                           s.mytaglist,
                           space_sep,
                           s.mypromptbox,
			},
			s.mytasklist, -- Middle widget
			{ -- Right widgets
				{
					space_sep,
					{
						{
							mykeyboardlayout,
							right=6,
							left=9,
							widget = wibox.container.margin,
						},
						shape  = my_parallelogram(3),
                                                forced_width = 35,
						bg = '#1a1823',
						fg = '#ffffff',
						widget = wibox.container.background,
					},
					{
						{
							{
								widget=pm
							},
							--pm_widget,
							right=10,
							left=7,
							widget = wibox.container.margin
						},
						shape  = my_parallelogram(3),
						bg = '#2b283a',
						fg = '#ffffff',
						widget = wibox.container.background,
					},
					{
						-- TODO: add margins if there are any apps in
						-- systray (if there are always added we get
						-- an ugly empty space)
						wibox.widget.systray(),
						widget = wibox.container.background,
					},
						{
							{
								volumecfg.widget,
								top = 2,
								bottom = 2,
								right=9,
								left=9,
								widget = wibox.container.margin
							},
							shape  = my_parallelogram(3),
							bg = '#3c3851',
							fg = '#ffffff',
							widget = wibox.container.background,
						},
						{
							{
								mytextclock,
								right=6,
								left=6,
								widget = wibox.container.margin
							},
							shape  = my_parallelogram(3),
							bg = '#4d4868',
							fg = '#ffffff',
							widget = wibox.container.background,

						},
						s.mylayoutbox,
						spacing = -6,
						layout  = wibox.layout.fixed.horizontal
					},
					top		= 3,
					bottom	= 3,
					widget	 = wibox.container.margin,
				},
			},
    }
end)
-- vim: set foldmethod=marker:
