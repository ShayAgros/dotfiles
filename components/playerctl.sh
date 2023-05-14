DEPS_APT=""

function check_if_installed() {
	if which playerctl >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Installing playerctl"

	mkdir -p ${SOFTWARE_DIR}/playerctl >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/playerctl

	wget --quiet https://github.com/altdesktop/playerctl/releases/download/v2.4.1/playerctl-2.4.1_amd64.deb \
		-O playerctl.deb
	sudo apt install ./playerctl.deb

	return 0
}
