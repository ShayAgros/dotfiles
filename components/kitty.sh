DEPS_APT=""

function check_if_installed() {

	if which kitty >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Installing kitty"

	mkdir -p ${SOFTWARE_DIR}/kitty >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/kitty

	wget --quiet https://github.com/kovidgoyal/kitty/releases/download/v0.28.1/kitty-0.28.1-x86_64.txz -O kitty.txz
	tar xf kitty.txz
	sudo ln -s `pwd`/bin/kitty ${BIN_INSTALL_DIR}/kitty
	
	echo Configuring kitty
	mkdir -p ~/.config/kitty || true
	ln -fs ${DOTS_DIR}/kitty.conf ~/.config/kitty/

	echo Downloading a different kitty icon
	git clone --quiet git@github.com:samholmes/whiskers.git
	sed -i -e '/Icon/d; $aIcon='`pwd`/whiskers/whiskers.png share/applications/kitty.desktop

	ln -fs `pwd`/share/applications/kitty.desktop ~/.local/share/applications/kitty.desktop

	return 0
}
