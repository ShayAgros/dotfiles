DEPS_APT=""

PIP_PACKAGES="neovim git-review cmake lxml requests libclang ply virtualenvwrapper"

PIP_PACKAGES=$(echo ${PIP_PACKAGES} | xargs)

function check_if_installed() {
	package_num=$(echo ${PIP_PACKAGES} | awk '{ print NF }' )

	# check if all the needed packages are installed
	if [[ $(pip3 list | grep -cE ${PIP_PACKAGES// /|} ) -eq ${package_num} ]]; then
		return 0
	fi

	return 1
}

function install_component() {

	echo "Installing pip3 packages"

	echo ${PIP_PACKAGES} | xargs -I {} -d ' ' pip3 install --user {}

	# configure virtual env
	echo > ~/.oh-my-zsh/custom/python_conf.zsh <<EOF
export WORKON_HOME=~/.local/envs
export VIRTUALENVWRAPPER_PYTHON=`which python3`
source `which virtualenvwrapper.sh`
EOF


	return 0
}
