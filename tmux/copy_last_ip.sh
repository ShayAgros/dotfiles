#!/usr/bin/env bash

scn_output=$(tmux capture-pane -p)

# use awk to remove duplicates
available_ips=$(echo "${scn_output}" | grep -oE '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+' | awk '!x[$0]++')

chosen_ip=$(echo "${available_ips}" | rofi -dmenu -i -p "Choose ip to copy")

if [[ -z ${chosen_ip} ]]; then
	exit 0
fi

#echo ${chosen_ip} > /tmp/result
echo -n ${chosen_ip} | xclip -selection clipboard
