#!/usr/bin/env bash

export SOFTWARE_DIR=~/workspace/software
export DOTS_DIR=`pwd`
export BIN_INSTALL_DIR=/usr/local/bin

function install_deps() {
	if which apt-get > /dev/null &&
	   [[ -n ${DEPS_APT} ]]; then
		DEPS_APT=$(echo $DEPS_APT | xargs)
		eval sudo apt-get install ${DEPS_APT}
	fi

	return 0
}

function configure_component() {
	comp=$1

	# if we were asked to install specific componenets, skip all
	# componenets that weren't requested
	if [[ -n ${COMPONENTS} ]] &&
	   ! $(echo ${COMPONENTS} | grep -q ${comp}) ; then
		return 0
	fi

	comp_file="./components/${comp}.sh"
	if [[ ! -f ${comp_file} ]]; then
		echo "ERROR: requested to install componenet ${comp}, but no ${comp_file} file was found"
		return 2
	fi

	# Install a componenet in a subshell to not clutter current environment
	# with components' envars
	(
		source ${comp_file}

		if [[ $(type -t check_if_installed) == function ]] &&
		   check_if_installed ; then
			echo "Component ${comp} is installed - skipping"
				return 0
		fi

		echo "===================== Installing ${comp} ====================="

		if [[ -n ${DEPS_APT} ]]; then
			echo "------ Installing dependencies ------"
			install_deps
		fi

		if [[ $(type -t install_component) == function ]]; then
			# break configuration if we failed a step
			set -e
			install_component
			set +e
		fi
	)

	return 0
}

trap "echo Failed to install component ${comp} ; exit 2" ERR

configure_component deps
configure_component amazon_apt
configure_component font
configure_component zsh
configure_component kitty
configure_component neovim
configure_component awesomewm
configure_component lua-language-server
configure_component node
configure_component tmux
configure_component playerctl
configure_component python # depends on oh-my-zsh
configure_component emacs
configure_component mu4e # depends on emacs
configure_component mbsync
