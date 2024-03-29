#!/usr/bin/env bash

get_ssh_connection() {
	pane_pid=${1}

	ssh_command=$(ps -o command= -g ${pane_pid} | awk '/^ssh/ {print $0 ; exit 0 }')

	[[ -z ${ssh_command} ]] && return

	# -G flag would cause alias to be resolved
	ssh_hostname=$(eval ${ssh_command} -G)
	ssh_hostname=$(echo "${ssh_hostname}" | awk '/^hostname/ {print $2}')

	ssh_entry=$(awk "/\y${ssh_hostname}\y/ {print NR}" ~/saved_instances/saved_logins)

	if [[ -n ${ssh_entry} ]]; then
		echo ${ssh_entry}
	fi
}


if [[ ${1} == "-w" ]]; then
	# send to window
	pane_pids=$(tmux list-panes -F '#{pane_pid}')
else
	# send to pane
	pane_pids=$(tmux display-message -p '#{pane_pid}')
fi

while read ppid ; do
	ssh_ixs="${ssh_ixs}\n$(get_ssh_connection ${ppid})"
done <<< "${pane_pids}"
ssh_ixs=$(echo -e "${ssh_ixs}" | sort -n | uniq | xargs)

if [[ -z ${ssh_ixs} ]]; then
	tmux display-message "ssh connection(s) wasn't identified"
	exit 0
fi

# replace spaces with commas
ssh_ixs=${ssh_ixs// /,}

# kinda ugly, but since the pane creates its own environment, anything that
# needs to be sequintial needs to be in it
tmux split-window -d -p 20 -f "~/workspace/scripts/send_file.sh ${ssh_ixs} || { tmux display-message \"Sending to ssh #${ssh_ixs} failed\" ; exit 0 ; } \
	&& tmux display-message \"Files sent to ssh #${ssh_ixs}\""
