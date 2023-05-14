DEPS_APT=""

function check_if_installed() {

	if grep -r "zim_transport" /etc/amazon | grep -q true ; then
		return 0
	fi

	return 1
}

function install_component() {

	sudo acw gir_transport true
	sudo acw zim_transport true

	sudo manage --force

	return 0
}
