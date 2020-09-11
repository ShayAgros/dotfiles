-------------------------------------------------
-- Gerrit Widget for Awesome Window Manager
-- Shows the number of currently assigned reviews
-- More details could be found here:
-- https://github.com/streetturtle/awesome-wm-widgets/tree/master/gerrit-widget

-- @author Pavel Makhov
-- @copyright 2019 Pavel Makhov
-------------------------------------------------

local awful = require("awful")
local wibox = require("wibox")
local watch = require("awful.widget.watch")
local json = require("json")
local spawn = require("awful.spawn")
local naughty = require("naughty")
local gears = require("gears")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

local mpopup = require("gerrit-widget.mpopup")

local HOME_DIR = os.getenv("HOME")

local GET_PASS_CMD = "gpg -q --for-your-eyes-only --no-tty -d ~/.emacs.d/.mbsyncpass.gpg | sed -n '6p'"

local GET_CHANGES_CMD = [[bash -c "curl  -s -X GET -u shayagr:$(%s) %s/a/changes/\\?q\\=%s | tail -n +2"]]
local GET_USER_CMD = [[bash -c "curl  -s -X GET -u shayagr:$(%s) %s/accounts/%s/ | tail -n +2"]]

local gerrit_widget = {}

local function create_popup_widget()

end

local function worker(args)

	local args = args or {}

	local icon = args.icons or HOME_DIR .. '/.config/awesome/gerrit-widget/gerrit_icon.svg'
	local host = args.host or naughty.notify{
		preset = naughty.config.presets.critical,
		title = 'Gerrit Widget',
		text = 'Gerrit host is unknown'
	}
	-- the reviewedby:self should show changed that I already reviewed
	local query = args.query or 'is:reviewer AND status:open AND NOT is:wip AND NOT reviewedby:self AND NOT owner:self'
	--local query = args.query or 'status:open AND project:ena-drivers'

	local current_number_of_reviews
	local previous_number_of_reviews = 0
	local name_dict = {}

	local rows = {
		layout = wibox.layout.fixed.vertical,
	}

	local popup = mpopup{
	--local popup = awful.popup{
		ontop = true,
		visible = false,
		shape = gears.shape.rounded_rect,
		border_width = 1,
		border_color = beautiful.bg_focus,
		maximum_height = 600,
		offset = { y = 5 },
		widget = {}
	}

	gerrit_widget = wibox.widget {
		{
			{
				image = icon,
				widget = wibox.widget.imagebox
			},
			margins = 4,
			layout = wibox.container.margin
		},
		{
			id = "txt",
			widget = wibox.widget.textbox
		},
		{
			id = "new_rev",
			widget = wibox.widget.textbox
		},
		layout = wibox.layout.fixed.horizontal,
		set_text = function(self, new_value)
			self.txt.text = new_value
		end,
		set_unseen_review = function(self, is_new_review)
			self.new_rev.text = is_new_review and '*' or ''
		end
	}

	local function get_name_by_user_id(user_id)
		if name_dict[user_id] == nil then
			name_dict[user_id] = {}
		end

		if name_dict[user_id].username == nil then
			name_dict[user_id].username = ''
			spawn.easy_async(string.format(GET_USER_CMD, GET_PASS_CMD, host, user_id), function(stdout, stderr, reason, exit_code)
				local user = json.decode(stdout)
				name_dict[tonumber(user_id)].username = user.name
			end)
			return name_dict[user_id].username
		end

		return name_dict[user_id].username
	end

	local update_widget = function(widget, stdout, _, _, _)
		-- remove this later
		local debug=0
		local reviews = {}
		if debug == 0 then
			if stdout == "" then
				widget:set_text("/offline/")
				return
			end


			reviews = json.decode(stdout)

			current_number_of_reviews = rawlen(reviews)

			if current_number_of_reviews == 0 then
				widget:set_visible(false)
				return
			end

			widget:set_visible(true)
			if current_number_of_reviews > previous_number_of_reviews then
				widget:set_unseen_review(true)
				naughty.notify{
					icon = HOME_DIR ..'/.config/awesome/gerrit-widget/gerrit_icon.svg',
					title = 'New Incoming Review',
					text = reviews[1].project .. '\n' .. get_name_by_user_id(reviews[1].owner._account_id) .. reviews[1].subject .. '\n',
					run = function() spawn.with_shell("xdg-open " .. host .. '/' .. reviews[1]._number) end
				}
			end

			previous_number_of_reviews = current_number_of_reviews
			widget:set_text(current_number_of_reviews)
		else
			table.insert(reviews, "item1")
			reviews[0] = "item1"
			reviews[1] = "item2"
			reviews[2] = "item3"
			reviews[3] = "item4"
			reviews[4] = "item5"
			reviews[5] = "item6"
			reviews[6] = "item7"
			reviews[7] = "item8"
			reviews[8] = "item9"
			reviews[9] = "item10"
			reviews[10] = "item11"
			reviews[11] = "item12"
			reviews[12] = "item13"
			widget:set_text(#reviews)
		end

		for i = 0, #rows do rows[i]=nil end
		for i, review in ipairs(reviews) do

			--local row = wibox.widget {
				--{
					--{
						--text = "    review number" .. i .. "    ",
						--widget = wibox.widget.textbox,
					--},
					--bottom = 8,
					--top = 8,
					--layout = wibox.container.margin
				--},
				--widget = wibox.container.background
			--}
			local row = wibox.widget {
				{
					{
						{
							markup = '<b>' .. review.project .. '</b>',
							align = 'center',
							widget = wibox.widget.textbox
						},
						{
							text = '  ' .. review.subject,
							widget = wibox.widget.textbox
						},
						{
							text = '  ' .. get_name_by_user_id(review.owner._account_id),
							widget = wibox.widget.textbox
						},
						vertical_spacing = 10,
						layout = wibox.layout.align.vertical
					},
					left = 20,
					bottom = 8,
					top = 8,
					layout = wibox.container.margin
				},
				widget = wibox.container.background
			}

			row:connect_signal("mouse::enter", function(c) c:set_bg(beautiful.bg_focus) end)
			row:connect_signal("mouse::leave", function(c) c:set_bg(beautiful.bg_normal) end)

			row:buttons(
			awful.util.table.join(
			awful.button({}, 1, function()
				spawn.with_shell("xdg-open " .. host .. '/' .. review._number)
				popup.visible = false
			end),
			awful.button({}, 3, function()
				spawn.with_shell("echo '" .. review._number .."' | xclip -selection clipboard")
				popup.visible = false
			end)
			)
			)

			table.insert(rows, row)
		end

		--local scrollbar = {
			--layout = wibox.container.scroll.vertical,
			--max_size = 300,
			--rows,
			--}
		local scrollbar = wibox.widget {
			layout = wibox.container.scroll.vertical,
			max_size = 300,
			step_function = wibox.container.scroll.step_functions
			.linear_increase,
			speed = 100,
			input_passthrough = true,
			rows
		}
		--scrollbar:setup(rows)
		--scrollbar:pause()
		--popup:setup({
			--layout = wibox.container.scroll.vertical,
			--max_size = 300,
			--step_function = wibox.container.scroll.step_functions
			--.linear_increase,
			--speed = 100,
			--rows,
			--})
		popup:setup(rows)
		--popup.widget = scrollbar
		scrollbar:pause()

		scrollbar:buttons (
			awful.util.table.join(
				awful.button({}, 1, function()
					scrollbar:continue()
					--scrollbar:pause()
				end)
			)
		)
	end

	gerrit_widget:buttons(
	awful.util.table.join(
		awful.button({}, 1, function()
			gerrit_widget:set_unseen_review(false)
			if popup.visible then
				popup.visible = not popup.visible
			else
				popup:move_next_to(mouse.current_widget_geometry)
			end
		end)
		)
	)

	watch(string.format(GET_CHANGES_CMD, GET_PASS_CMD, host, query:gsub(" ", "+")), 10, update_widget, gerrit_widget)
	return gerrit_widget
end

return setmetatable(gerrit_widget, { __call = function(_, ...) return worker(...) end })
