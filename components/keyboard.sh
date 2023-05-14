function check_if_installed() {
	if [[ -f /etc/X11/xorg.conf.d/00-keyboard.conf ]]; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Configuring keyboard"

	sudo mkdir /etc/X11/xorg.conf.d 2>/dev/null || true
	sudo cp -i ${DOTS_DIR}/00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf
	sudo cp -i ${DOTS_DIR}/30-touchpad.conf /etc/X11/xorg.conf.d/30-touchpad.conf

	return 0
}
