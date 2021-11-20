#!/usr/bin/env bash

tmux_ver=$(tmux -V | grep -Eo '[0-9\.]+')
# TODO: add support for tmux 3.2 where we can synchronize by pane and not only
# window

# TODO: make is support the four left-most panes each time and not just 4 panes
for i in {1..4}; do
	panes[${i}]=$(tmux list-panes -F '#{pane_top} #{pane_left} #{pane_index} #{pane_id}' | sort -n | awk "NR==${i} {print \$4}")
done

#echo first: ${first}
#echo second: ${second}
#echo third: ${third}
#echo fourth: ${fourth}
window_sync_state=$(tmux show-options -w synchronize-panes | awk '{print $2}')

tmux set-window-option synchronize-panes off

for i in {1..4}; do
	#was_synched=
	#tmux send-keys -t ${panes[${i}]} "echo ${i}" "Enter"
	tmux send-keys -t ${panes[${i}]} "Escape" "${i}" "Escape" "i"
done

# apparently if you never set this option, it might not be defined
if [[ -n ${window_sync_state} ]]; then
	tmux set-window-option synchronize-panes ${window_sync_state}
fi
