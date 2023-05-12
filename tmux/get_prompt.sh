#!/usr/bin/env bash

pane_pid=${1}
current_dir=${2}

get_ssh_connection() {
	ssh_login=$(ps -o command= -g ${pane_pid} | awk '/^ssh/ {print $NF ; exit 0 }')

	if [[ -z ${ssh_login} ]]; then
		project=$(echo -n "${current_dir}" | grep -oE 'patches/[^/]+' | sed -E 's?patches/([a-z_]+)?\1?')
		echo "Running in $(pwd)" >> /tmp/my_log
		echo "project is ${project}" >> /tmp/my_log
		[[ -n ${project} ]] && cprompt="${project::8}: "
		cprompt="${cprompt}$(basename ${current_dir})"
		echo ${cprompt}
	else
		ssh_login=$(echo ${ssh_login} | sed 's/[^@]\+@\([^@]\+\)/\1/')
		ssh_entry=$(awk "/\y${ssh_login}\y/ {print NR}" ~/saved_instances/saved_logins)

		if [[ -z ${ssh_entry} ]]; then
			echo "ssh: ${ssh_login}"
		else
			echo "ssh: #${ssh_entry}"
		fi
	fi
}

#if [[ -z ${pane_pid} ]]; then
	#basename ${pwd}
	#exit 0
#fi

#prompt=$(get_ssh_connection)

#echo pid: ${pane_pid}
get_ssh_connection
