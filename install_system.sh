#!/usr/bin/env

export SOFTWARE_DIR=~/workspace/software
export DOTS_DIR=`pwd`

function install_deps() {
	if which apt-get > /dev/null &&
	   [[ -n ${DEPS_APT} ]]; then
		DEPS_APT=$(echo $DEPS_APT | xargs)
		eval sudo apt-get install ${DEPS_APT}
	fi

	return 0
}

function install_componenet() {
	comp=$1

	# if we were asked to install specific componenets, skip all
	# componenets that weren't requested
	if [[ -n ${COMPONENTS} ]] &&
	   ! $(echo ${COMPONENTS} | grep -q ${comp}) ; then
		return 0
	fi

	comp_file="./componenets/${comp}.sh"
	if [[ ! -f ${comp_file} ]]; then
		echo "ERROR: requested to install componenet ${comp}, but no ${comp_file} file was found"
	fi

	# Install a componenet in a subshell to not clutter current environment
	# with components' envars
	(
		source ${comp_file}
		
		
	)
}
